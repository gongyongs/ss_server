%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 七月 2014 下午6:24
%%%-------------------------------------------------------------------
-module(session_work).
-author("zqlt").

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
-include("../cache/cache.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(EXPIRED_TIME, 3 * 60 * 60).
-define(CLAEN_EXPIRED_TIME, 10*1000).

-define(ONLINE_USER_STATISTIC, 10*60).

-define(KEY, "YUMSAGA").

-export([
  login/4,
  get_uin_by_session/3,
  timer_func/2,
  delete_session_by_uin/1
]).

-record(state, {session_seed = 1, last_refresh_ts}).
-record(session, {session_id, uin, node, add_time}).
-record(uin_to_sid, {uin, session_id}).

%%%===================================================================
%%% API
%%%===================================================================
login(Uin, Node, Ip, Device) ->
  gen_server:call(?MODULE, {login, {Uin, Node, Ip, Device}}).

get_uin_by_session(SessionID, Node, RequestType) ->
  case ets:lookup(session, SessionID) of
    [#session{uin = Uin, node = Node} = Session] ->
      gen_server:cast(?MODULE, {refresh_time, Session, RequestType}),
      {success, Uin};
    [#session{node = Other}] ->
      ?FILE_LOG_ERROR("other=~p", [Other]),
      fail;
    [] -> fail
  end.

delete_session_by_uin(Uin) ->
  gen_server:call(?MODULE, {delete_session, Uin}).
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
  ets:new(session, [set, protected, named_table, {keypos, #session.session_id}]),
  ets:new(uin_to_sid, [set, protected, named_table, {keypos, #uin_to_sid.uin}]),
  clean_expired_session(),
  {ok, #state{session_seed = 1, last_refresh_ts = 0}}.

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
handle_call({login, {Uin, Node, Ip, Device}}, _From, #state{session_seed = SessionSeed} = State) ->
  case ets:lookup(uin_to_sid, Uin) of
    [#uin_to_sid{session_id = Sid}] ->
      ?FILE_LOG_DEBUG("delete [~p, ~p]", [Uin, Sid]),
      ets:delete(session, Sid);
    [] -> ok
  end,
  {Session, NewSessionSeed} = generate_session(SessionSeed),
  ets:insert(session, #session{session_id = Session, uin = Uin, node = Node, add_time = dd_util:timestamp()}),
  ets:insert(uin_to_sid, #uin_to_sid{uin = Uin, session_id = Session}),
  session_statistics:login(Uin, 0, Ip, Device),
  {reply, {success, Session}, State#state{session_seed = NewSessionSeed}};
handle_call({delete_session, Uin}, _From, State) ->
  case ets:lookup(uin_to_sid, Uin) of
    [#uin_to_sid{session_id = Sid}] ->
      ?FILE_LOG_DEBUG("delete [~p, ~p]", [Uin, Sid]),
      ets:delete(session, Sid),
      ets:delete(uin_to_sid, Uin);
    [] ->
      ?FILE_LOG_DEBUG("use sid not exist", [])
  end,
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
handle_cast({refresh_time, Session, RequestType}, State) ->
  Dif = dd_util:timestamp() - Session#session.add_time,
  session_statistics:add_operation_time(Session#session.uin, Dif, RequestType),
  case ets:update_element(session, Session#session.session_id, [{#session.add_time, dd_util:timestamp()}]) of
    true ->
      ?FILE_LOG_DEBUG("update [~p]", [Session]);
    false ->
      ?FILE_LOG_DEBUG("not update [~p]", [Session])
  end,
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
handle_info({timeout, _TimerRef, 'clear_expired_session'}, #state{last_refresh_ts = LastFreshTime} = State) ->             %%清除旧的session，并统计现有的人数
  CurTime = dd_util:timestamp(),
  ?FILE_LOG_DEBUG("clean session", []),
 %% ?FILE_LOG_DEBUG("session table all: [~p]", [ets:tab2list(session)]),

  %%每天凌晨某个时刻强制删除所有数据，让用户重新登陆
  {success, FreshTime} = check_login_refresh_session(LastFreshTime, CurTime),
  Tree =
    lists:foldr(
      fun(#session{session_id = Sid, uin = Uin, node = Node, add_time = AddTime}, TmpTree) ->
        if
          CurTime - AddTime >= ?EXPIRED_TIME ->  %%session过期时间
            ?FILE_LOG_DEBUG("session timeout [~p]", [Sid]),
            ets:delete(session, Sid),
            ets:delete(uin_to_sid, Uin),
            TmpTree;
          true ->
            ?FILE_LOG_DEBUG("not time out session [~p]", [Sid]),
            if
              CurTime - AddTime > ?ONLINE_USER_STATISTIC -> %%默认为不在线
                TmpTree;
              true ->
                case gb_trees:lookup(Node, TmpTree) of
                  none -> gb_trees:insert(Node, 1, TmpTree);
                  {value, Count} -> gb_trees:update(Node, Count + 1, TmpTree)
                end
            end
        end
      end, gb_trees:empty(), ets:tab2list(session)),

%%  ?FILE_LOG_DEBUG("session table all: [~p]", [ets:tab2list(session)]),

  lists:foreach(
    fun({Node, OnlineCount}) ->
      spawn(
        fun() ->
          rpc:cast(Node, gateway_monitor, set_online_user_count, [OnlineCount])
        end)
    end, gb_trees:to_list(Tree)),
%%  ?FILE_LOG_DEBUG("session table all: [~p]", [ets:tab2list(session)]),
  clean_expired_session(),
  {noreply, State#state{last_refresh_ts = FreshTime}};
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
  ?FILE_LOG_DEBUG("TERMINATE!!![~p]", [Reason]),
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
generate_session(SessionSeed) when SessionSeed < 16#FFFFFFFF ->
  CurTime = dd_util:timestamp(),
  Val = ?KEY ++ dd_util:to_list(CurTime) ++ dd_util:to_list(SessionSeed),
  {generate_session(dd_util:md5_string(Val), dd_util:to_list(SessionSeed), CurTime rem 32), SessionSeed + 1};
generate_session(_) ->
  CurTime = dd_util:timestamp(),
  Val = ?KEY ++ dd_util:to_list(CurTime) ++ "1",
  {generate_session(dd_util:md5_string(Val), "1", CurTime rem 32), 2}.

generate_session(MD5, Seed, Max) ->
  generate_session_1(MD5, Seed, 0, Max, []).
generate_session_1([], _, _, _, OutList) -> lists:reverse(lists:flatten(OutList));
generate_session_1([H | T], Seed, Index, Max, OutList) ->
  if
    Index =:= Max -> generate_session_1(T, Seed, Index + 1, Max, [Seed | OutList]);
    true -> generate_session_1(T, Seed, Index + 1, Max, [H | OutList])
  end.

clean_expired_session() ->
  erlang:start_timer(?CLAEN_EXPIRED_TIME, self(), 'clear_expired_session').

timer_func(ParentPid, WaitTime) ->
  receive
    {modify_time, Time} -> timer_func(ParentPid, Time);
    _ -> timer_func(ParentPid, WaitTime)
  after
    WaitTime * 1000 ->	%%一分钟执行一次
      ParentPid ! clean,
      timer_func(ParentPid, WaitTime)
  end.

check_login_refresh_session(LastFreshTime, CurTime) ->
  if
    LastFreshTime =:= 0 ->
      ?FILE_LOG_DEBUG("FRESH SESSION", []),
      ets:delete_all_objects(session),
      ets:delete_all_objects(uin_to_sid),
      ?FILE_LOG_DEBUG("session statistics ", []),
      session_statistics:output_operation_log(),
      session_statistics:next_day_clear(),
      {success, CurTime};
    true ->
      {C, T} = dd_util:to_local_time(dd_util:timestamp_to_datetime(CurTime)),
      %%
      case dd_util:time_compare_by_datetime({C, T}, {C, ?LOGIN_REWARD_FRESH_TIME}) of
        -1 -> {success, LastFreshTime};
        _ -> %%比较上次刷新时间，查看是否已经刷新过
          LastFreshDateTime = dd_util:to_local_time(dd_util:timestamp_to_datetime(LastFreshTime)),
          case dd_util:time_compare_by_datetime(LastFreshDateTime, {C, ?LOGIN_REWARD_FRESH_TIME}) of
            -1 ->
              ets:delete_all_objects(session),
              ets:delete_all_objects(uin_to_sid),
              ?FILE_LOG_DEBUG("FRESH SESSION", []),
              session_statistics:output_operation_log(),
              session_statistics:next_day_clear(),
              {success, CurTime};
            _ -> {success, LastFreshTime}
          end
      end
  end.
