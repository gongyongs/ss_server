%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 十一月 2014 下午2:25
%%%-------------------------------------------------------------------
-module(login_work).
-author("zqlt").

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
-include("../../deps/hash_service/src/hash_service.hrl").
-include("login.hrl").
%% API
-export([start_link/0]).
-export([
  execute/4
]).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([
  update_hash_rule/2
]).

-export([
  login_by_uname/3,
  login_by_device/3,
  update_by_device/2,
  update_by_uname/2,
  device_work_proc/0,
  uname_work_proc/0,
  data_trans/2
]).

-define(SERVER, ?MODULE).
-define(POOL_SIZE, 5).
-record(state, {uname_work_pool, device_work_pool}).
-define(RD_EXPIRED_TIME, (8 * 60 * 60)).
%%%===================================================================
%%% API
%%%===================================================================
execute(Key, ModName, Func, Param) when is_atom(ModName) andalso is_atom(Func) ->
  gen_server:call(?MODULE, {event, Key, {ModName, Func, Param}}).

data_trans(Source, DataList) when is_list(DataList) ->
  gen_server:cast(?MODULE, {data_trans, {Source, DataList}}).

update_hash_rule(IsAffected, LoginRule) when is_record(LoginRule, hash_rule) ->
  gen_server:call(?MODULE, {update_hash_rule, {IsAffected, LoginRule}}, infinity).

login_by_uname(UName, PlatProcName, LoginInfo) when is_list(UName) andalso is_atom(PlatProcName) ->
  gen_server:call(?MODULE, {uname_req, {UName, PlatProcName, {login_by_uname, LoginInfo}}}).

login_by_device(Device, PlatProcName, LoginInfo) when is_list(Device) andalso is_atom(PlatProcName) ->
  gen_server:call(?MODULE, {device_req, {Device, PlatProcName, {login_by_device, LoginInfo}}}).

update_by_device(Device, PlatProcName) when is_list(Device) andalso is_atom(PlatProcName) ->
  gen_server:call(?MODULE, {device_req, {Device, PlatProcName, {update_by_device, Device}}}).

update_by_uname(UName, PlatProcName) when is_list(UName) andalso is_atom(PlatProcName) ->
  gen_server:call(?MODULE, {uname_req, {UName, PlatProcName, {update_by_uname, UName}}}).

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
  ets:new(login_cfg, [set, protected, named_table, {keypos, #login_cfg.key}]),
  {success, DBNode} =  dd_ms:read_config(database_node),
  {success, TokenNode} = dd_ms:read_config(token_node),
  ets:insert(login_cfg, #login_cfg{key=database_node, value = DBNode}),
  ets:insert(login_cfg, #login_cfg{key=token_node, value = TokenNode}),
  UNameWorkPool = create_pool(?POOL_SIZE, uname_work_proc, []),
  DeviceWorkPool = create_pool(?POOL_SIZE, device_work_proc, []),
  {ok, #state{uname_work_pool = UNameWorkPool, device_work_pool = DeviceWorkPool}}.

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
handle_call({uname_req, {Key, Mod, Event}}, From, #state{uname_work_pool = WorkPool}=State) ->
  Work = get_pool_work(Key, WorkPool),
  Work ! {event, {From, Mod, Event}},
  {noreply, State};
handle_call({device_req, {Key, Mod, Event}}, From, #state{device_work_pool = WorkPool}=State) ->
  Work = get_pool_work(Key, WorkPool),
  Work ! {event, {From, Mod, Event}},
  {noreply, State};
handle_call({update_hash_rule, {false, LoginRule}}, _From, State) ->
  %%更新hash规则，不锁定
  ?FILE_LOG_DEBUG("update_hash_rule not lock [~p]", [LoginRule]),
  ets:insert(login_cfg, #login_cfg{key = hash_rule, value = LoginRule}),
  {reply, success, State};
handle_call({update_hash_rule, {true, LoginRule}}, _From, #state{device_work_pool = {DeviceTreePool, _}, uname_work_pool = {UNameTreePool, _}} = State) ->
  %%需要用新的哈希规则将数据迁移到别的节点上
  L1 = gb_trees:to_list(DeviceTreePool),
  L2 = gb_trees:to_list(UNameTreePool),
  LL = [L1, L2],
  lists:foreach(
    fun(L) ->
      lists:foreach(
        fun({_, Pid}) ->
          Pid ! {lock, self()}
        end, L),
      lists:foreach(
        fun({_, Pid}) ->
          receive
            {lock_success, Pid} -> ok
          end
        end, L)
    end, LL),

  ?FILE_LOG_DEBUG("update_hash_rule lock success [~p]", [LoginRule]),
  ets:insert(login_cfg, #login_cfg{key = hash_rule, value = LoginRule}),
  %%锁定成功之后
  %%开始传输数据
  lists:foreach(
    fun(L) ->
      lists:foreach(
        fun({_, Pid}) ->
          Pid ! {data_trans, self()}
        end, L),

      lists:foreach(
        fun({_, Pid}) ->
          receive
            {data_trans_success, Pid} -> ok
          end
        end, L)
    end, LL),
  ?FILE_LOG_DEBUG("trans_data success [~p]", [LoginRule]),
  %%等待传输完成
  {reply, success, State};

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
handle_cast({data_trans, {{uname, SourceNode}, DataList}}, #state{uname_work_pool = WorkPool} = State) ->
  ?FILE_LOG_DEBUG("uname source_node[~p] data_trans len[~p]", [SourceNode, length(DataList)]),
  lists:foreach(
    fun(Data) ->
      Work = get_pool_work(Data#user_info.uname, WorkPool),
      Work ! {insert_rd, Data}
    end, DataList),
  {noreply, State};
handle_cast({data_trans, {{device, SourceNode}, DataList}}, #state{device_work_pool = WorkPool} = State) ->
  ?FILE_LOG_DEBUG("device source_node[~p] data_trans len[~p]", [SourceNode, length(DataList)]),
  lists:foreach(
    fun(Data) ->
      Work = get_pool_work(Data#user_info.device, WorkPool),
      Work ! {insert_rd, Data}
    end, DataList),
  {noreply, State};
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
create_pool(PoolSize, Func, WorkParam) ->
  TreePool =
    lists:foldr(
      fun(Index, TmpTree) ->
        Pid = spawn(?MODULE, Func, WorkParam),
        gb_trees:insert(Index, Pid, TmpTree)
      end, gb_trees:empty() ,lists:seq(0, PoolSize - 1)),
  {gb_trees:balance(TreePool), PoolSize}.

%%通过Key获取一个work
get_pool_work(Key, {PoolTree, PoolSize}) when is_list(Key) ->
  HashValue = hash_service_util:hash_string(Key),
  RemValue = HashValue rem PoolSize,
  gb_trees:get(RemValue, PoolTree).

device_work_proc() ->
  TableID  = ets:new(user_info, [set, protected, {keypos, #user_info.device}]),
  put(table_id, TableID),
  put(mode, device),
  work_proc_1().

uname_work_proc() ->
  TableID = ets:new(user_info, [set, protected, {keypos, #user_info.uname}]),
  put(table_id, TableID),
  put(mode, uname),
  work_proc_1().

work_proc_1() ->
  receive
    {event, {From, Mod, {Func, FuncParam}}} ->
      RetValue =
        try
          Mod:execute(Func, FuncParam)
        catch
          throw:{custom, Reason} ->
            ?FILE_LOG_ERROR("[~p][~p][~p] error, reason = ~p", [Mod, Func, FuncParam, Reason]),
            {fail, Reason};
          What:Type ->
            ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
            {fail, "HintSystemError"}
        end,
      gen_server:reply(From, RetValue),
      work_proc_1();
    {lock, From} ->
      ?FILE_LOG_DEBUG("~p lock", [From]),
      From ! {lock_success, self()},
      work_proc_1();
    {data_trans, From} ->
      %%数据转移
      ?FILE_LOG_DEBUG("start data_trans ~p.", [self()]),
      try
        login_work_impl:data_trans()
      catch
        throw:{custom, Reason} ->
          ?FILE_LOG_WARNING("custom reason=~p", [Reason]),
          {fail, Reason};
        What:Type ->
          ?FILE_LOG_ERROR(
            "what=~p, type=~p, stack=~p.",
            [What, Type, erlang:get_stacktrace()]),
          {fail, "logic error"}
      end,
      From ! {data_trans_success, self()},
      work_proc_1();
    {insert_rd, Data} ->
      TableId = get(table_id),
      ets:insert(TableId, Data#user_info{add_ts = dd_util:timestamp()}),
      work_proc_1();
    Other ->
      ?FILE_LOG_ERROR("login_work =>  error request: [~p]", [Other]),
      success
  after
    5*60*1000 ->
      %%存储设备登陆的缓存
      TableId = get(table_id),
      CurTime = dd_util:timestamp() - ?RD_EXPIRED_TIME,
      MatchSpec = [{#user_info{add_ts = '$1', _='_'}, [{'=<', '$1', CurTime}], [true]}],
      DeleteCount = ets:select_delete(TableId, MatchSpec),
      if
        DeleteCount > 0 ->
          ?FILE_LOG_DEBUG("delete Expired device rd ~p", [DeleteCount]);
        true -> ok
      end,
      work_proc_1()
  end.