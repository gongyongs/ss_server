%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 十二月 2014 下午2:09
%%%-------------------------------------------------------------------
-module(log_db).

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([
  work_proc/1,
  execute_tower/2,
  execute_tower_login/2
]).
-define(SERVER, ?MODULE).
-define(DB_POOL_COUNT, 2).
-define(DB_WORK_POOL_COUNT, 2).

-define(DEFAULT_DATABASE, log_tower).
-define(DATABASE_TOWER_LOGIN,log_tower_login).

-record(state, {current_db, work_proc_pool,work_proc_tower_login_pool}).

%%%===================================================================
%%% API
%%%===================================================================
execute_tower(FunName, FunParam) ->
  gen_server:call(?MODULE, {execute_tower, {FunName, FunParam}}).

execute_tower_login(FunName, FunParam) ->
  gen_server:call(?MODULE, {execute_tower_login, {FunName, FunParam}}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
%%初始化连接池
init([]) ->
  connect_db(?DB_POOL_COUNT),
  WorkProcPool = create_work_pool(?DB_WORK_POOL_COUNT, [?DEFAULT_DATABASE]),
  WorkProcPool1 = create_work_pool_tower_login(?DB_WORK_POOL_COUNT,[?DATABASE_TOWER_LOGIN]),
  {ok, #state{current_db = ?DEFAULT_DATABASE, work_proc_pool = WorkProcPool,work_proc_tower_login_pool = WorkProcPool1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({execute_tower, {Name, Param}}, From, #state{work_proc_pool = WorkPool}=State) ->
  {Work, NewWorkPool} = get_work_proc(WorkPool),
  ?FILE_LOG_DEBUG("execute_tower => Work is ~p, NewWorkPool is ~p",[Work, NewWorkPool]),
  Work ! {execute, {From, {Name, Param}}},
  {noreply, State#state{work_proc_pool = NewWorkPool}};
handle_call({execute_tower_login, {Name, Param}}, From, #state{work_proc_tower_login_pool = WorkPool}=State) ->
  {Work, NewWorkPool} = get_work_proc_tower_login(WorkPool),
  ?FILE_LOG_DEBUG("execute_tower_login => Work is ~p, NewWorkPool is ~p",[Work, NewWorkPool]),
  Work ! {execute, {From, {Name, Param}}},
  {noreply, State#state{work_proc_tower_login_pool = NewWorkPool}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
log(Moudle, Line, _Level, FormatFun) ->
  {Format, Arguments} = FormatFun(),
  file_log_server:send(debug, self(), Moudle, Line, Format, Arguments).

connect_db(PoolCount) ->
  {success, DbCfgList} = dd_config:get_cfg(db_list2),
  %%开启进程，连接数据库
  spawn(
    fun() ->
      lists:map(
        fun({SqlName, {Ip, Port, Account, Password, DbName}}) ->
          mysql:start(SqlName, Ip, Port,  Account, Password, DbName, fun log/4),
          lists:foreach(
            fun(_) ->
              Result = mysql:connect(SqlName, Ip, Port, Account, Password, DbName, undefined, true, false),
              ?FILE_LOG_DEBUG("mysql_connect Result is ~p",[Result])
            end, lists:seq(1, PoolCount - 1))
        end, DbCfgList)
    end
  ).

create_work_pool(PoolCnt, Param) when PoolCnt > 1 ->
  TreePool =
    lists:foldr(
      fun(Index, TmpTree) ->
        Pid = spawn(?MODULE, work_proc, Param),
        gb_trees:insert(Index, Pid, TmpTree)
      end, gb_trees:empty(), lists:seq(1, PoolCnt)),
  ?FILE_LOG_DEBUG("tree pool = ~p", [TreePool]),
  {gb_trees:balance(TreePool), 1, PoolCnt}.

create_work_pool_tower_login(PoolCnt, Param) when PoolCnt > 1 ->
  TreePool =
    lists:foldr(
      fun(Index, TmpTree) ->
        Pid = spawn(?MODULE, work_proc, Param),
        gb_trees:insert(Index, Pid, TmpTree)
      end, gb_trees:empty(), lists:seq(11, 10+PoolCnt)),
  ?FILE_LOG_DEBUG("tree pool = ~p", [TreePool]),
  {gb_trees:balance(TreePool), 11, 10+PoolCnt}.

get_work_proc({Tree, CurWorkIdx, MaxWorkSize}) ->
  WorkPid = gb_trees:get(CurWorkIdx, Tree),
  NewWorkIdx = CurWorkIdx rem MaxWorkSize,
  {WorkPid, {Tree, NewWorkIdx + 1, MaxWorkSize}}.

get_work_proc_tower_login({Tree, CurWorkIdx, MaxWorkSize}) ->
  WorkPid = gb_trees:get(CurWorkIdx, Tree),
  NewWorkIdx = CurWorkIdx rem MaxWorkSize,
  {WorkPid, {Tree, NewWorkIdx + 11, MaxWorkSize}}.

%%处理逻辑进程，一个最简单的server模式
work_proc(DbName) ->
  receive
    {change_db_name, NewDbName} ->
      ?FILE_LOG_DEBUG("[~p] change db_name to [~p]", [DbName, NewDbName]),
      work_proc(NewDbName);
    {execute, {From, {FuncName, FuncParam}}} when is_atom(FuncName) ->
      RetValue =
        try
          log_db_proc:execute(DbName, {FuncName, FuncParam})
        catch
          throw:{custom, Reason} -> {fail, Reason};
          What:Type ->
            ?FILE_LOG_ERROR("what=~p, type=~p, stack=~p", [What, Type, erlang:get_stacktrace()]),
            {fail, "logic error"}
        end,
      gen_server:reply(From, RetValue),
      work_proc(DbName);
    Other ->
      ?FILE_LOG_ERROR("other request [~p]", [Other]),
      work_proc(DbName)
  end.