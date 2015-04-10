%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 六月 2014 下午5:43
%%%-------------------------------------------------------------------
-module(adminserver_cache_proxy).
-author("yaohong").

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

-export([rpc_cache/3]).
-export([rpc_mail_call/2]).
-export([rpc_stopserver_call/1]).
-export([add_admin/2, get_admin/1,stop_server/1]).
-export([get_restart_node/1]).
-define(SERVER, ?MODULE).

-record(state, {cache_nodes}).
-record(admin_cfg, {key, value}).
-record(admin_rd, {oper_code, user_name, timestamp}).


-define(ADMIN_TIME, (60 * 60)).
-define(CLEAN_ADMIN_SPACE, 10 * 1000).


rpc_mail_call(FuncName, FuncParam) ->
  MailNode = get_mail_node(),
  ?FILE_LOG_DEBUG("adminserver_cache_proxy=>get_mail_node ok , MailNode is ~p, FuncParam is ~p", [MailNode,FuncParam]),
  Result =
    try
      case rpc:call(MailNode, mail, FuncName, FuncParam) of
        {badrpc, Reason} ->
          ?FILE_LOG_WARNING("rpc::call fail reason=~p", [Reason]);
        success ->	{success, "success"};
        {success,Val} -> {success,Val};
        {fail, Reason} -> {fail, Reason};
        fail -> {fail, "operation failed"}
      end
    catch
      throw:{custom, Reason1} ->
        ?FILE_LOG_WARNING("rpc_mail_call fail reason=~p", [Reason1]),
        {fail, rpc_exception};
      What:Type ->
        ?FILE_LOG_ERROR("what=~p, type=~p, stack=~p", [What, Type, erlang:get_stacktrace()]),
        {fail, sys_exception}
    end,
  Result.

rpc_cache(Uin, FuncName, FuncParam) ->
  {success, CacheNode} = adminserver_util:get_cache_node(Uin),
    try
      rpc_cache_1(CacheNode, FuncName, FuncParam)
    catch
      throw:{custom, rpc_exception} ->
        ?FILE_LOG_WARNING("rpc_cache fail reason=rpc_exception", []),
        {fail, rpc_exception};
      What:Type ->
        ?FILE_LOG_ERROR("what=~p, type=~p, stack=~p", [What, Type, erlang:get_stacktrace()]),
        {fail, sys_exception}
    end.


rpc_cache_1([], _, _) -> throw({custom, rpc_exception});
rpc_cache_1([Node|Nodes], FuncName, FuncParam) ->
  case rpc:call(Node, db_cache_api, FuncName, FuncParam) of
    {badrpc, Reason} ->
      ?FILE_LOG_WARNING("rpc::call fail reason=~p", [Reason]),
      rpc_cache_1(Nodes, FuncName, FuncParam);
    Other -> Other
  end.


get_cache_nodes() ->
  [#admin_cfg{value = CacheNodes}] = ets:lookup(admin_cfg, cache_nodes),
  CacheNodes.
get_mail_node() ->
  [#admin_cfg{value = MailNode}] = ets:lookup(admin_cfg, mail_node),
  MailNode.

rpc_stopserver_call(Stop_time)->
  gen_server:call(?MODULE,{stop_server,Stop_time}).

add_admin(OperCode, UName) ->
  gen_server:cast(?MODULE, {add_admin, {OperCode, UName}}).

get_admin(OperCode) ->
  case ets:lookup(admin_rd, OperCode) of
    [] -> fail;
    [#admin_rd{oper_code = OperCode, user_name = UserName}] ->
      {success, UserName}
  end.


get_restart_node(NodeIndex)->
	case NodeIndex of
		1->
			get_mail_node();
		_->
			get_mail_node()
	end.

%%%===================================================================
%%% API
%%%===================================================================

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
  {success, CacheNodes} = dd_ms:get_all_cache(),
  {success, MailNode} = dd_ms:read_config(mail_node),
  ets:new(admin_cfg, [set, protected, named_table, {keypos, #admin_cfg.key}]),
  ets:insert(admin_cfg, #admin_cfg{key = cache_nodes, value = CacheNodes}),
  ets:insert(admin_cfg, #admin_cfg{key = mail_node,value = MailNode}),
  ets:new(admin_rd, [set, protected, named_table, {keypos, #admin_rd.oper_code}]),

  start_register_time(),
  {ok, #state{cache_nodes = CacheNodes}}.

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
handle_call({stop_server,Stop_time}, _From, State) ->           %stop_server,Stop_time      RPid = spawn(fun() -> Pid !{self(), settlement_one_server_rank(Item, ServerCount)} end),
%    SelfPid = self(),
  spawn(?MODULE, stop_server, [Stop_time]),
 %% spawn(fun() -> stop_server(Stop_time) end),
  {reply, success, State};

handle_call(_Request, _From, State) ->
  {reply, fail, State}.
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
handle_cast({add_admin, {OperCode, UName}}, State) ->
  ?FILE_LOG_DEBUG("add_admin oper_code=~p uname=~p", [OperCode, UName]),
  ets:insert(admin_rd, #admin_rd{oper_code = OperCode, user_name = UName, timestamp = dd_util:timestamp()}),
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
handle_info({timeout, _TimerRef, 'clear_expired_admin'}, State) ->
  clear_expired_admin(),
  start_register_time(),
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
  ?FILE_LOG_DEBUG("terminate!!!! ~p", [Reason]),
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


start_register_time() ->
  erlang:start_timer(?CLEAN_ADMIN_SPACE, self(), 'clear_expired_admin').


clear_expired_admin() ->
  CurTime = dd_util:timestamp() - ?ADMIN_TIME,
  MatchSpec = [{#admin_rd{timestamp = '$1', _='_'}, [{'=<', '$1', CurTime}], [true]}],
  ets:select_delete(admin_rd, MatchSpec).


stop_server(Stop_time)->
  {success, SessionNode} = dd_ms:read_config(session_node),
  [Ye,Mo,Da,Ho,Mi,Se] = string:tokens(Stop_time,"/"),
  Time =  {{dd_util:to_integer(Ye), dd_util:to_integer(Mo), dd_util:to_integer(Da)}, {dd_util:to_integer(Ho),dd_util:to_integer(Mi),dd_util:to_integer(Se)}},
  receive
    _ -> ok
  after
    10000 ->
      {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
      case dd_util:time_compare_by_datetime({{Year, Month, Day}, {Hour, Minute, Second}}, Time) of   %大于Time时返回1
        1 -> %%达到关闭服务器时间
          case rpc:call(SessionNode, session, stop, []) of
            ok ->
              ?FILE_LOG_INFO("STOP SESSION SUCCESS!", []);
            Other ->
              ?FILE_LOG_DEBUG("stop session error, ~p",[Other])
          end,
          success;
        _ ->
          ?FILE_LOG_DEBUG("not the time", []),
          stop_server(Stop_time)
      end
  end.


