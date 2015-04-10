%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 七月 2014 下午8:23
%%%-------------------------------------------------------------------
-module(gateway_monitor).
-author("zqlt").

-behaviour(gen_server).
-include("../dd_ms.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-include("../../deps/hash_service/src/hash_service.hrl").
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
  set_online_user_count/1,
  get_online_user_count/0,
   update_cache_hash_rule/1
]).
-define(SERVER, ?MODULE).

-define(UPDATE_ONLINE_USER_COUNT, 10*1000).
-define(MAX_USER_COUNT, 100000).

-record(state, {current_online_count = 0}).

set_online_user_count(OnlineNumber) when is_integer(OnlineNumber)->
  gen_server:cast(?MODULE, {set_online_user_count, OnlineNumber}).

get_online_user_count() ->
  gen_server:call(?MODULE, get_online_user_count).

update_cache_hash_rule(CacheHashRule) when is_record(CacheHashRule, hash_rule) ->
  gen_server:call(?MODULE, {update_cache_hash_rule, CacheHashRule}).

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
  {success, SessionNode} = dd_ms:read_config(session_node),
  start_register_timer(),
  dd_config:write_cfg(session_node, SessionNode),
  {ok, #state{current_online_count = 0}}.

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
handle_call(get_online_user_count, _From, #state{current_online_count = OnlineUser} = State) ->
  {reply, {success, OnlineUser}, State};
handle_call({update_cache_hash_rule, LoginHashRule}, _From, State) ->
  dd_config:write_cfg(cache_hash_rule, LoginHashRule),
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
handle_cast({set_online_user_count, OnlineNumber}, State) ->
  ?FILE_LOG_DEBUG("SET_ONLINE_USER_COUNT: onlineNumber = ~p", [OnlineNumber]),
  {noreply, State#state{current_online_count = OnlineNumber}};
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
handle_info({timeout, _TimerRef, 'update_online_user_count'}, #state{current_online_count = CurOnlineCount} = State) ->
  spawn(
    fun() ->
      {success, {Ip, Port}} = dd_config:get_cfg(external_addr),
      StrIp = dd_util:ipv4_to_str(Ip),
      GateWay = #gateway{node = node(), info = #gateway_info{cur_user_count = CurOnlineCount, max_user_count = ?MAX_USER_COUNT, gateway_addr = #gateway_addr{ip = StrIp, port = Port}}},
      %%?FILE_LOG_DEBUG("update online user count: handle_timeout, external_addr:~p, ~p, gateway: ~p, onlineCount: ~p", [Ip, Port, GateWay,CurOnlineCount]),
      success = dd_ms:write_gateway(GateWay)
  end),
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
  ?FILE_LOG_ERROR("terminate error, reason = ~p", [Reason]),
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
start_register_timer() ->
  erlang:start_timer(?UPDATE_ONLINE_USER_COUNT, self(), 'update_online_user_count').