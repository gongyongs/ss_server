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
-include("../../deps/file_log/include/file_log.hrl").
 -include("../dd_ms.hrl").
-include("../../deps/hash_service/src/hash_service.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).
 -export([node_up/1, node_down/1,write_config/0]).
-define(SERVER, ?MODULE).

-record(state, {login_hash_rule}).
%%%===================================================================
%%% API
%%%===================================================================
node_up(Node) ->
   gen_server:cast(?MODULE, {node_up, Node}).

node_down(Node) ->
  gen_server:cast(?MODULE, {node_down, Node}).
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
  write_config(),
  {ok, #state{login_hash_rule = #hash_rule{node_tree = gb_trees:empty(), rem_value_list = []}}}.

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
handle_cast({node_up, Node}, #state{login_hash_rule = LoginHashRule} = State) ->
   ANode = atom_to_list(Node),
  [Prefix, _] = string:tokens(ANode, "@"),
  [_Head, Type_Name, _Index] = string:tokens(Prefix, "_"),
  ?FILE_LOG_DEBUG("nodeup type_name=~p", [Node]),

  if
    Type_Name =:= "login" ->
      %%login%%
     dd_ms:write_login(#login{node = Node, info = #login_info{}}),
      %%有节点加入，更新哈希规则
      {AffectedNodeList ,NewLoginHashRule} = hash_service_util:update_hash_rule(Node, LoginHashRule),
      update_login_hash_rule(AffectedNodeList ,NewLoginHashRule),
     {noreply, State#state{login_hash_rule = NewLoginHashRule}};
    Type_Name =:= "dispatch" ->
      %%dispatch%%
      dd_ms:write_dispatch(#dispatch{node = Node, info = #dispatch_info{}}),
      case rpc:call(Node, dispatch_monitor, update_login_hash_rule, [LoginHashRule]) of
        success ->
          ?FILE_LOG_DEBUG("update_login_hahs_rule to node[~p] success", [Node]);
        Other ->
          ?FILE_LOG_DEBUG("update_login_hahs_rule to node[~p]  fail reason=~p", [Node, Other])
      end,
      {noreply, State};
    true ->
      {noreply,State}
  end;
handle_cast({node_down, Node}, #state{login_hash_rule = LoginHashRule} = State) ->
  ANode = atom_to_list(Node),
  [Prefix, _] = string:tokens(ANode, "@"),
  [_Head, TypeName, _Index] = string:tokens(Prefix, "_"),
  ?FILE_LOG_DEBUG("nodedown type_name=~p", [Node]),

  if
    TypeName =:= "login" ->
      %%login%%
      dd_ms:del_login(Node),
      NewHashRule = hash_service_util:update_hash_rule(Node, LoginHashRule, remove),
      update_login_hash_rule([] ,NewHashRule),
      {noreply, State#state{login_hash_rule = NewHashRule}};
    TypeName =:= "dispatch" ->
      %%dispatch%%
      dd_ms:del_dispatch(Node),
      {noreply, State};
    true ->
      {noreply,State}
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
  ?FILE_LOG_ERROR("master monitor terminate => reason = ~p", [Reason]),
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
	io:format("now  in write_config --->~p~n",[DataList]),
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


%%更新hash规则
update_login_hash_rule(AffectedLoginNodeList ,NewLoginHashRule) ->
  try
    {success, LoginList} = dd_ms:get_all_login(),
    {success, DispatchList} = dd_ms:get_all_dispatch(),
    SelfPid = self(),
    PidList1 =
      lists:map(
        fun(Login) ->
          spawn(
            fun() ->
              IsAffected = lists:member(Login#login.node, AffectedLoginNodeList),
              %%受影响了
              case rpc:call(Login#login.node, login_work, update_hash_rule, [IsAffected, NewLoginHashRule]) of
                success -> ok;
                Other ->
                  ?FILE_LOG_ERROR("update hash_rule to node[~p] fail reason=~p", [Login#login.node, Other])
              end,
              SelfPid ! {update_hash_rule_end, self()}
            end)
        end, LoginList),
    lists:foreach(
      fun(Pid1) ->
        receive
          {update_hash_rule_end, Pid1} -> ok
        end
      end, PidList1),
    PidList2 =
      lists:map(
        fun(Dispatch) ->
          spawn(
            fun() ->
              %%受影响了
              case rpc:call(Dispatch#dispatch.node, dispatch_monitor, update_login_hash_rule, [NewLoginHashRule]) of
                success -> ok;
                Other ->
                  ?FILE_LOG_ERROR("update hash_rule to node[~p] fail reason=~p", [Dispatch#dispatch.node, Other])
              end,
              SelfPid ! {update_hash_rule_end, self()}
            end)
        end, DispatchList),
    lists:foreach(
      fun(Pid2) ->
        receive
          {update_hash_rule_end, Pid2} -> ok
        end
      end, PidList2),
    success
  catch
    What:Type ->
      ?FILE_LOG_ERROR("update_login_hash_rule exception what=~p, type=~p, stack=~p", [What, Type, erlang:get_stacktrace()]),
      fail
  end.


