%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 七月 2014 下午8:46
%%%-------------------------------------------------------------------
-module(token_cache).
-author("zqlt").

-behaviour(gen_server).

-include("token.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-include("../dispatch/dispatch.hrl").
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
  add_login_token/3,
  get_login_token/1,
  get_login_token/2,
  work_proc/0]).

-export([
  get_version_info/1,
  update_version_info/1
]).

-define(SERVER, ?MODULE).
-define(POOL_SIZE, 5).
-record(state, {work_pool}).


%%%===================================================================
%%% API
%%%===================================================================
add_login_token(Uin, Token, LoginInfo) ->
  gen_server:call(?MODULE, {add_login_token, {Uin, Token, LoginInfo}}).

get_login_token(Uin) ->
  get_login_token(Uin, true).

get_login_token(Uin, IsDelete) ->
  gen_server:call(?MODULE, {get_login_token, {Uin, IsDelete}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_version_info(VersionID) when is_list(VersionID) ->
  gen_server:call(?MODULE, {get_version_info, VersionID}).

update_version_info(VersionInfo) when is_record(VersionInfo, version)->
  gen_server:call(?MODULE, {update_version_info, VersionInfo}).

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
init([]) ->
  WorkPool = create_work_pool(?POOL_SIZE, work_proc, []),
  ets:new(version_info, [set, protected, named_table, {keypos, #version.version_id}]),
  {ok, #state{work_pool = WorkPool}}.

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
handle_call({get_version_info, VersionID}, _From, State) ->
  Ret =
    case ets:lookup(version_info, VersionID) of
      [] ->
        {success, DbNode} = dd_ms:read_config(database_node),
        case rpc:call(DbNode, database_monitor, execute, [get_version_info, VersionID]) of
          not_exist ->
            {success, #version{version_id = VersionID, version_package_url = "", version_update_url = ""}};
          {success, VersionInfo} ->
            ets:insert(version_info, VersionInfo),
            {success, VersionInfo};
          Other ->
            ?FILE_LOG_ERROR("query version info error, reason = ~p", [Other]),
            {fail, "HintSystemDataError"}
        end;
      [Version] -> {success, Version}
    end,
  {reply, Ret, State};
handle_call({update_version_info, VersionInfo}, _From, State) ->
  Ret =
    case ets:lookup(version_info, VersionInfo#version.version_id) of
      [] ->
        ets:insert(version_info, VersionInfo),
        success;
      [VersionInfo] ->
        ets:insert(version_info, VersionInfo),
        success
    end,
  {reply, Ret, State};
handle_call({add_login_token, {Uin, Token, LoginInfo}}, From, #state{work_pool = WorkPool} = State) ->
  Work = get_pool_work(Uin, WorkPool),
  Work ! {event, From, {add_login_token, {Uin, Token, LoginInfo}}},
  {noreply, State};
handle_call({get_login_token, {Uin, IsDelete}}, From, #state{work_pool = WorkPool} = State) ->
  Work = get_pool_work(Uin, WorkPool),
  Work ! {event, From, {get_login_token, {Uin, IsDelete}}},
  {noreply, State};

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
create_work_pool(PoolCnt, Func, Param) ->
   TreePool =
    lists:foldr(
      fun(Index, TmpTree) ->
        Pid = spawn(?MODULE, Func, Param),
        gb_trees:insert(Index, Pid, TmpTree)
      end, gb_trees:empty(), lists:seq(0, PoolCnt)),
  {gb_trees:balance(TreePool), PoolCnt}.

get_pool_work(Uin, {PoolTree, PoolSize}) ->
  RemValue = Uin rem PoolSize,
  gb_trees:get(RemValue, PoolTree).

work_proc() ->
   TableId = ets:new(token_rd, [set, protected, {keypos, #token_rd.uin}]),
   put(table_id, TableId),
   work_proc_1().

work_proc_1() ->
  receive
    {event, From, {FuncName, FunParam}} ->
      RetValue =
        try
          token_impl:execute(FuncName, FunParam)
        catch
          {custom, FailReason} ->
            ?FILE_LOG_WARNING("custom reason=~p", [FailReason]),
            {fail, FailReason};
          What:Type ->
            ?FILE_LOG_ERROR(
              "what=~p, type=~p, stack=~p.",
              [What, Type, erlang:get_stacktrace()]
            ),
            {fail, "logic error"}
        end,
      gen_server:reply(From, RetValue),
      work_proc_1();
  Other ->
    ?FILE_LOG_WARNING("other msg ~p", [Other])
  after
    ?TIMEOUT ->
      %%超时了，删除过期的token
      TableId = get(table_id),
      CurTime = dd_util:timestamp() - ?TOKEN_EXPIRED_TIME,
      MatchSpec = [{#token_rd{add_time  = '$1', _='_'}, [{'=<', '$1', CurTime}], [true]}],
      DeleteCount = ets:select_delete(TableId, MatchSpec),
      if
        DeleteCount > 0 ->
          ?FILE_LOG_DEBUG("delete Expired rd ~p", [DeleteCount]);
        true -> ok
      end,
      work_proc_1()
  end.
