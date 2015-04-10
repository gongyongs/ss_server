%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 七月 2014 下午5:22
%%%-------------------------------------------------------------------
-module(master_monitor).
-author("zqlt").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-include("../../deps/file_log/include/file_log.hrl").
-include("../dd_ms.hrl").
-include("../../deps/hash_service/src/hash_service.hrl").

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([
  node_up/1,
  node_down/1
]).

-export([
  reload_config/0
]).

-define(SERVER, ?MODULE).

-define(REGISTER_SPAN, 30 * 1000).

-record(state, {cache_hash_rule}).
-record(service, {node :: node(), type :: atom()}).
%%%===================================================================
%%% API
%%%===================================================================
node_up(Node) when is_atom(Node) ->
  gen_server:cast(?MODULE, {node_up, Node}).

node_down(Node) when is_atom(Node) ->
  gen_server:cast(?MODULE, {node_down, Node}).

reload_config() ->
  gen_server:call(?MODULE, reload_config).
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
  ok = net_kernel:monitor_nodes(true),
  start_register_timer(), %%向dispatch服务器注册游戏逻辑服
  ets:new(?MODULE, [set, named_table, protected, {keypos, #service.node}]),
  write_config(),
  {ok, #state{cache_hash_rule = #hash_rule{node_tree = gb_trees:empty(), rem_value_list = []}}}.

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
handle_call(reload_config, _From, State) ->
  Ret =
    try
      write_config()
    catch
      What:Type ->
        ?FILE_LOG_ERROR("reload config error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
        fail
    end,
  {reply, Ret, State};
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
handle_cast({node_up, Node}, #state{cache_hash_rule = CacheHashRule} = State) ->
  StrNode = atom_to_list(Node),
  [Prefix, _] = string:tokens(StrNode, "@"),
  [_Head, TypeName, _Index] = string:tokens(Prefix, "_"),
  ?FILE_LOG_DEBUG("node up  type name = ~p", [TypeName]),

  if
    TypeName =:= "cache" ->
      success = dd_ms:write_cache(#cache{node = Node, info = #cache_info{}}),
      %%新节点加入，更新算法
      {AffectedNodeList ,NCacheHashRule} = hash_service_util:update_hash_rule(Node, CacheHashRule),
      update_cache_hash_rule(AffectedNodeList, NCacheHashRule),
      success = dd_ms:write_config(cache_hash_rule, NCacheHashRule),
      ?FILE_LOG_DEBUG("node up cache", []),
      {noreply, State#state{cache_hash_rule = NCacheHashRule}};
    TypeName =:= "gateway" ->
      success = dd_ms:write_gateway(#gateway{node = Node, info = #gateway_info{}}),
      case rpc:call(Node, gateway_monitor, update_cache_hash_rule, [CacheHashRule]) of
        success ->
          ?FILE_LOG_DEBUG("update_cache_hash_rule to node[~p] success", [Node]);
        Other ->
          ?FILE_LOG_DEBUG("update_cache_hash_rule to node[~p]  fail reason=~p", [Node, Other])
      end,
      ?FILE_LOG_DEBUG("node up gateway", []),
      {noreply, State};
    true ->
      {noreply, State}
  end;
handle_cast({node_down, Node}, #state{cache_hash_rule = CacheHashRule} = State) ->
  StrNode = atom_to_list(Node),
  [Prefix, _] = string:tokens(StrNode, "@"),
  [_Head, TypeName, _Index] = string:tokens(Prefix, "_"),
  ?FILE_LOG_DEBUG("node dwon  type name = ~p", [TypeName]),

  if
    TypeName =:= "cache" ->
      dd_ms:del_cache(Node),
      NHashRule = hash_service_util:update_hash_rule(Node, CacheHashRule, remove),
      update_cache_hash_rule([] ,NHashRule),
      success = dd_ms:write_config(cache_hash_rule, NHashRule),
      {noreply, State#state{cache_hash_rule = NHashRule}};
    TypeName =:= "gateway" ->
      dd_ms:del_gateway(Node),
      {noreply, State};
    true ->
      {noreply, State}
  end;
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

handle_info({timeout, _TimerRef, 'register_to_dispatch'}, State) ->
  spawn(fun() -> register_to_dispatch() end),
  ?FILE_LOG_DEBUG("timer out  register_to_dispatch", []),
  start_register_timer(),
  {noreply, State};
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
terminate(Reason, _State) ->
  ?FILE_LOG_ERROR("master monitor terminate, reason = ~p", [Reason]),
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
write_config() ->
    PublicCfgPath = os:getenv("PUBLIC_CFG_PATH"),
    {ok, [{mode, Mode}, {config_data, DataList}]} = file:consult(PublicCfgPath),
    success = dd_ms:write_config(mode, Mode),
    lists:foreach(
      fun(Data) ->
        case Data of
          {Mode, ParamList} ->
            lists:foreach(
             fun({Key, Value}) ->
               success = dd_ms:write_config(Key, Value)
             end, ParamList);
          _ -> ok
        end
        end, DataList),
  ok.

start_register_timer() ->
  erlang:start_timer(?REGISTER_SPAN, self(), 'register_to_dispatch').

register_to_dispatch() ->
  try
    {success, GateWays} = dd_ms:get_all_gateway(),
    %%?FILE_LOG_DEBUG("register_to_dispatch: Gateways = ~p", [GateWays]),
    GateWays2 =
      lists:filter(
      fun(GateWay) ->
        if
          GateWay#gateway.info =/= undefined -> true;
          true -> false
        end
      end, GateWays),
    %%?FILE_LOG_DEBUG("register_to_dispatch FILTER: Gateways2 = ~p", [GateWays2]),

    if
      GateWays2 =/= [] ->
          SortFunc =
          fun(#gateway{info = #gateway_info{cur_user_count = LCnt}}, #gateway{info = #gateway_info{cur_user_count = RCnt}}) ->
            if
              LCnt > RCnt -> false;
              true -> true
            end
          end,

        [TmpGateWay | _] = lists:sort(SortFunc, GateWays2),
        GatewayAddr = TmpGateWay#gateway.info#gateway_info.gateway_addr,
        {success, {ServerIndex, ServerName, ServerType}} = dd_ms:read_config(server_info),
        {success, {DispatchSeverIP, DispatchServerPort}} = dd_ms:read_config(dispatch_server),
        Value = ["id=", dd_util:to_list(ServerIndex), "&name=", ServerName, "&type=", dd_util:to_list(ServerType),"&ip=",GatewayAddr#gateway_addr.ip, "&port=", dd_util:to_list(GatewayAddr#gateway_addr.port), "&state=1"],
        ReqUrl = lists:flatten(["http://", DispatchSeverIP, ":", dd_util:to_list(DispatchServerPort), "/game_server_register"]),

        %%?FILE_LOG_DEBUG("request url: ~p", [ReqUrl]),

        _ReqValue = httpc:request('post', {ReqUrl, [], "application/x-www-form-urlencoded", iolist_to_binary(lists:flatten(Value))}, [], []),
        ok;
      true -> ok
    end
  catch
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()])
  end.

%%更新cache和网关接口的hash规则
update_cache_hash_rule(AffectedCacheNodeList, CacheHashRule) ->
  try
    {success, CacheNodeList} = dd_ms:get_all_cache(),
    {success, GateWayNodeList} = dd_ms:get_all_gateway(),
    SelfPid = self(),
    CachePidList =
      lists:map(
        fun(Cache) ->
          spawn(
            fun() ->
              IsAffected = lists:member(Cache#cache.node, AffectedCacheNodeList),
              case rpc:call(Cache#cache.node, cache_data_mgr, update_hash_rule, [IsAffected, CacheHashRule]) of
                success -> ok;
                Other ->
                  ?FILE_LOG_ERROR("update hash rule to node ~p fail reason = ~p", [Cache#cache.node, Other])
              end,
              SelfPid ! {update_hash_rule_end, self()}
            end)
        end, CacheNodeList),
    lists:foreach(
      fun(Pid) ->
        receive
          {update_hash_rule_end, Pid} -> ok
        end
      end, CachePidList),
    GatewayPidList =
      lists:map(
        fun(GatewayNode) ->
          spawn(
            fun() ->
              case rpc:call(GatewayNode#gateway.node, gateway_monitor, update_cache_hash_rule, [CacheHashRule]) of
                success -> ok;
                Other ->
                  ?FILE_LOG_ERROR("update hash rule to node ~p, fail reason = ~p", [GatewayNode#gateway.node, Other])
              end,
              SelfPid ! {update_hash_rule_end, self()}
            end)
        end, GateWayNodeList),
    lists:foreach(
      fun(Pid) ->
        receive
          {update_hash_rule_end, Pid} -> ok
        end
      end, GatewayPidList),
    success
  catch
    What:Type ->
      ?FILE_LOG_ERROR("update_cache_hash_rule exception what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      fail
  end.