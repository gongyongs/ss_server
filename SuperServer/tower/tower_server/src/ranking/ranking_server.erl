%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 九月 2014 下午4:42
%%%-------------------------------------------------------------------
-module(ranking_server).
-author("zqlt").

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
-include("../cache/cache_def.hrl").
-include("../dd_ms.hrl").
-include("ranking.hrl").

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
  get_server_ranking/1,
  ranking_work_proc/1,
  get_all_rank_info/0,
  server_ranking_settlement/2,
  manual_ranking_settlement/0
]).

-define(SERVER, ?MODULE).
%%每周天的凌晨三点点更新时间
-define(RANK_SETTLEMENT_DAY, 7).
-define(RANK_SETTLEMENT_TIME, {3, 0, 0}).
%%单次读取的数据条数
-define(DB_SINGLE_MAX_READ, 500).

%%同时进行计算的进程数
-define(MAX_PROCESS_COUNT, 50).
%%玩家信息
-record(user_info, {
  uin::integer(),
  uname::string(),
  dis_name::string(),
  point::integer(),
  gain_ts::integer()
}).
%%玩家到分数的影射
-record(uin_to_rank_rd,{
  uin::integer(),
  rank::integer(),
  point::integer()
}).
%%分数到玩家信息的影射
-record(point_to_user_info_rd, {
  point::integer(),
  user_info::[#user_info{}]
}).
%%排名到分数的影射
-record(rank_to_point, {
  rank::integer(),
  point::integer()
}).

%%排名到玩家的影射
-record(rank_to_user,{
  rank::integer(),
  user_info::#user_info{}
}).

-record(state, {
  cur_get_rank_pid::pid(),
  work_pid_1::pid(),
  work_pid_2::pid(),
  last_settement_ts::integer()   %%上次结算的时间
}).



%%%===================================================================
%%% API
%%%===================================================================
get_server_ranking(Uin) ->
  gen_server:call(?MODULE, {get_ranking, Uin}).

get_all_rank_info() ->
  gen_server:call(?MODULE, get_all_rank_info).

manual_ranking_settlement() ->
  gen_server:call(?MODULE, ranking_settlement).
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
  SelfPid = self(),
  Pid1 = spawn(?MODULE, ranking_work_proc, [SelfPid]),
  Pid2 = spawn(?MODULE, ranking_work_proc, [SelfPid]),
  erlang:start_timer(1*60000, self(), 'start_ranking'),
  erlang:start_timer(5*60000, self(), 'server_ranking_settement'),  %%结算
  {ok, #state{cur_get_rank_pid = 0, work_pid_1 = Pid1, work_pid_2 = Pid2, last_settement_ts = dd_util:timestamp()}}.

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
handle_call({get_ranking, Uin}, From, #state{cur_get_rank_pid = CurPid}=State) ->
  if
    CurPid =:= undefined ->
      {reply, fail, State};
    CurPid =:= 0 ->
      Ret = {success, {[], {1,0,1}}},
      {reply, Ret, State};
    true ->
      CurPid ! {{get_ranking, Uin}, From},
      {noreply, State}
  end;
handle_call(get_all_rank_info, From, #state{cur_get_rank_pid = CurPid}=State) ->
  if
    CurPid =:= undefined ->
      {reply, fail, State};
    CurPid =:= 0 ->
      Ret = {success, {[], {1,0,1}}},
      {reply, Ret, State};
    true ->
      CurPid ! {get_all_rank_info, From},
      {noreply, State}
  end;
handle_call(ranking_settlement, _From, State) ->
  SelfPid = self(),
  spawn(?MODULE, server_ranking_settlement, [SelfPid, 0]),
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
handle_cast(_Request, State) ->
  {noreply, State}.


-define(LOAD_SPACE_TIME, (12 *60 * 60 * 1000)).

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
handle_info({timeout, _TimerRef, 'start_ranking'}, #state{work_pid_1 = P1, work_pid_2 = _P2} = State) ->
  P1 ! read_db,
  {noreply, State#state{cur_get_rank_pid = P1}};
handle_info({timeout, _TimerRef, 'server_ranking_settement'}, #state{last_settement_ts = LastSettlementTs} = State) ->
  SelfPid = self(),
  spawn(?MODULE, server_ranking_settlement, [SelfPid, LastSettlementTs]),
  erlang:start_timer(30*60000, self(), 'server_ranking_settement'),  %%结算
  {noreply, State};
handle_info({load_complete, Pid}, #state{work_pid_1 = P1,work_pid_2 = P2} = State) ->
  NPid =
    if
      Pid =:= P1 -> P2;
      Pid =:= P2 -> P1
    end,
  ?FILE_LOG_DEBUG("pid[~p] load_complete, next_load_pid=~p", [Pid, NPid]),
  erlang:start_timer(?LOAD_SPACE_TIME, NPid, read_db),
  {noreply, State#state{cur_get_rank_pid = Pid}};
handle_info(settlement_finish, State) ->
  ?FILE_LOG_DEBUG("settlement finish", []),
  {noreply, State#state{last_settement_ts = dd_util:timestamp()}};
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

ranking_work_proc(Pid) ->
  UinToRankRd = ets:new(uin_to_rank_rd, [set, protected, {keypos, #uin_to_rank_rd.uin}]),
  PointToUserInfoRd = ets:new(point_to_user_info_rd, [set, protected, {keypos, #point_to_user_info_rd.point}]),
  RankToPoint = ets:new(rank_to_point, [set, protected, {keypos, #rank_to_point.rank}]),
  RankToUser = ets:new(rank_to_user, [set, protected, {keypos, #rank_to_user.rank}]),
  ranking_work_proc({UinToRankRd, PointToUserInfoRd, RankToPoint, RankToUser}, Pid).
ranking_work_proc(TableInfo, Pid) ->
  try
    receive
      read_db ->
        %%读取db
        ?FILE_LOG_DEBUG("pid[~p], read_db", [self()]),
        clear_table(TableInfo),
        case read_db() of
          {success, DataList} ->
            write_ets(TableInfo, DataList),
            ?FILE_LOG_DEBUG("pid[~p] read_db success", [self()]),
            Pid ! {load_complete, self()};
          fail ->
            ?FILE_LOG_DEBUG("pid[~p] read_db fail", [self()]),
            timer:sleep(60*1000),
            self() ! read_db
        end,
        ranking_work_proc(TableInfo, Pid);
      {timeout, _TimerRef, read_db} ->
        self() ! read_db,
        ranking_work_proc(TableInfo, Pid);
      {{get_ranking, Uin}, From} ->
        spawn(
          fun() ->
            Ret = get_ranking_1(TableInfo, Uin),
            gen_server:reply(From, Ret)
          end),
        ranking_work_proc(TableInfo, Pid);
      {get_all_rank_info, From} ->
        spawn(
          fun() ->
            Ret = get_all_rank_info(TableInfo),
            gen_server:reply(From, Ret)
          end),
        ranking_work_proc(TableInfo, Pid);
      _ -> ranking_work_proc(TableInfo, Pid)
    end
  catch
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      ranking_work_proc(TableInfo, Pid)
  end.

read_db() ->
  case read_db_to_list(0, ?DB_SINGLE_MAX_READ, []) of
    {success, DataList} ->
      {success, merge(DataList)};
    fail -> fail
  end.

read_db_to_list(Index, Len, OutList) ->
  {success, DbNode} = dd_ms:read_config(database_node),
  case rpc:call(DbNode, database_monitor, execute, [get_user_stage_info, {Index, Len}]) of
    {success, DataList} ->
      EndlessDataList =
        lists:map(
          fun({Uin, Platform, Stage}) ->
            {Uin, Platform#platform_info.player_id, Platform#platform_info.player_dis_name,
                Stage#stage.endless_tollgate#endless_tollgate.max_score, Stage#stage.endless_tollgate#endless_tollgate.max_score_gain_ts}
          end, DataList),
      SortFunc =
        fun({_, _, _, LPoint, LTs}, {_, _, _, RPoint, RTs}) ->
          if
            LPoint > RPoint -> true;
            LPoint =:= RPoint ->
              if
                LTs =< RTs -> true;
                true -> false
              end;
            true -> false
          end
        end,
      SortAfterList = lists:sort(SortFunc, EndlessDataList),
      LL = length(SortAfterList),
      if
        LL < Len -> {success, [SortAfterList | OutList]};
        true -> read_db_to_list(Index + Len, Len, [SortAfterList | OutList])
      end;
    fail ->
      ?FILE_LOG_ERROR("read_to_db_list fail", []),
      fail;
    _ -> fail
  end.


clear_table({UinToRankRd, PointToUserInfoRd, RankToPoint, RankToUser})  ->
  ets:delete_all_objects(UinToRankRd),
  ets:delete_all_objects(PointToUserInfoRd),
  ets:delete_all_objects(RankToPoint),
  ets:delete_all_objects(RankToUser).


%%采用多进程处理，并且保证接收每一个进程的结果是按顺序的
merge(InList) ->
  OutPidList = merge_sort(InList, [], ?MAX_PROCESS_COUNT),
  SortRetList =
    lists:foldr(
      fun(Pid, TmpRetList) ->
        receive
          {Pid, {sort_ret, SortRet}} ->
            [SortRet | TmpRetList]
        after
          5*1000 ->
            %%超时
            ?FILE_LOG_ERROR("sort ret timeout", []),
            TmpRetList
        end
      end, [], OutPidList),
  case SortRetList of
    [] -> throw("sort exception");
    [SortRetItem] -> SortRetItem;
    SortRetList -> merge(SortRetList)
  end.

merge_sort([], OutPidList, _ProcessCount) -> OutPidList;
merge_sort(InList, OutPidList, 0) ->
  merge_sort(InList, OutPidList, ?MAX_PROCESS_COUNT);
merge_sort(InList, OutPidList, ProcessCount) when length(InList) =:= 1 ->
  merge_sort([[] | InList], OutPidList, ProcessCount);
merge_sort([L1, L2 | T], OutPidList, ProcessCount) ->
  Pid = self(),
  RPid = spawn(fun() -> Pid ! {self(), {sort_ret, merge_1(L1, L2, [])}} end),
  merge_sort(T, [RPid | OutPidList], ProcessCount - 1).

merge_1([], [], OutList) -> lists:reverse(OutList);
merge_1([], R, OutList) ->  lists:reverse(OutList) ++ R;
merge_1(L, [], OutList) -> lists:reverse(OutList) ++ L;
merge_1([{_, _, _, LV, LTS} = L | LT], [{_, _, _, RV, RTS} = R | RT], OutList) when LV =:= RV ->
  if
    LTS =< RTS ->  merge_1(LT, [R |RT], [L | OutList]);
    true ->  merge_1([L | LT], RT, [R | OutList])
  end;
merge_1([{_, _, _, LV, _} = L | LT], [{_, _, _, RV, _} = R | RT], OutList) when LV > RV -> merge_1(LT, [R |RT], [L | OutList]);
merge_1([{_, _, _, LV, _} = L | LT], [{_, _, _, RV, _} = R | RT], OutList) when LV < RV -> merge_1([L | LT], RT, [R | OutList]).


write_ets(TableInfo, SortAfterList) ->
  write_ets(TableInfo, SortAfterList, 1).

write_ets(_TableInfo, [], _) -> ok;
write_ets({UinToRankRd, PointToUserInfoRd, RankToPoint, RankToUser} = TableInfo, [{Uin, UName, DisName, Point, GainTs} | SortAfterList], Rank) ->
  case ets:lookup(PointToUserInfoRd, Point) of
    [] ->
      ets:insert(PointToUserInfoRd, #point_to_user_info_rd{point = Point, user_info = [#user_info{uin = Uin, point = Point, gain_ts = GainTs, uname = UName, dis_name = DisName}]}),
      ets:insert(UinToRankRd, #uin_to_rank_rd{uin = Uin, point = Point,rank = Rank}),
      ets:insert(RankToPoint, #rank_to_point{rank = Rank, point = Point}),
      ets:insert(RankToUser,#rank_to_user{rank = Rank, user_info = #user_info{point = Point, uin = Uin, gain_ts = GainTs, uname = UName, dis_name = DisName}}),
      write_ets(TableInfo, SortAfterList, Rank + 1);
    [RankRd] ->
      ets:insert(UinToRankRd, #uin_to_rank_rd{uin = Uin, point = Point,rank = Rank}),
      ets:insert(RankToUser,#rank_to_user{rank = Rank, user_info = #user_info{point = Point, uin = Uin, gain_ts = GainTs, uname = UName, dis_name = DisName}}),
      ets:update_element(
        PointToUserInfoRd,
      RankRd#point_to_user_info_rd.point,
      [{#point_to_user_info_rd.user_info, [#user_info{uin = Uin, point = Point, gain_ts = GainTs, uname = UName, dis_name = DisName} | RankRd#point_to_user_info_rd.user_info]}]),
      write_ets(TableInfo, SortAfterList, Rank + 1)
  end.


get_ranking_1({UinToRankRd, _PointToUserInfoRd, _RankToPoint, RankToUser} = TableInfo, Uin) ->
  try
    Top20List = get_top_twenty(TableInfo),
    PlayerCount = dd_util:get_ets_size(RankToUser),
    if
      PlayerCount =:= 0 -> throw({custom, "logic error"});
      true -> ok
    end,
    case ets:lookup(UinToRankRd, Uin) of
      [] -> %%不在榜单上
        {success, {Top20List, {0, 0, PlayerCount}}};
      [#uin_to_rank_rd{rank = Rank, point = Point}] ->
        {success, {Top20List, {Rank, Point, PlayerCount}}}
    end
  catch
    {custom, Reason} -> {fail, Reason};
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      {fail, "logic error"}
  end.

%%获取前二十名
get_top_twenty({_UinToRankRd, _PointToUserInfoRd, _RankToPoint, RankToUser} = _TableInfo) ->
  get_top_twenty_1(RankToUser, 1, []).
get_top_twenty_1(_RankToUser, 21, OutList) -> lists:reverse(OutList);
get_top_twenty_1(RankToUser, CurRank, OutList) ->
  case ets:lookup(RankToUser, CurRank) of
    [] ->
      lists:reverse(OutList);
    [Item] ->
      get_top_twenty_1(RankToUser, CurRank + 1, [{Item#rank_to_user.user_info#user_info.uin, Item#rank_to_user.user_info#user_info.uname,
        Item#rank_to_user.user_info#user_info.dis_name, Item#rank_to_user.user_info#user_info.point, Item#rank_to_user.rank} | OutList])
  end.

get_all_rank_info({_UinToRankRd, _PointToUserInfoRd, _RankToPoint, RankToUser}) ->
  try
    AllRankList = ets:tab2list(RankToUser),
    RankList =
      lists:map(
        fun(Item) ->
          {Item#rank_to_user.user_info#user_info.uin, Item#rank_to_user.user_info#user_info.point, Item#rank_to_user.rank}
        end, AllRankList),
    {success, RankList}
  catch
    {custom, Reason} -> {fail, Reason};
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      {fail, "logic error"}
  end.

server_ranking_settlement(ParentPid, LastTs) ->
  case should_be_settlement(LastTs) of
    false -> ok;
    true ->
      ?FILE_LOG_DEBUG("START SETTLEMENT!", []),
      settlement_server_ranking(),
      settlement_friend_rank(),
      ParentPid ! settlement_finish
  end.

should_be_settlement(LastSettlementTs) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
  WeekTh = calendar:day_of_the_week(Year, Month, Day),
  if
    WeekTh =/= ?RANK_SETTLEMENT_DAY -> false;
    true ->
      %%是否到结算时间
      case dd_util:time_compare_by_datetime({{Year, Month, Day}, {Hour, Minute, Second}}, {{Year, Month, Day}, ?RANK_SETTLEMENT_TIME}) of
        1 -> %%达到结算时间,判断上一次结算时间
          LastSettlementDateTime = dd_util:to_local_time(dd_util:timestamp_to_datetime(LastSettlementTs)),
          case dd_util:time_compare_by_datetime(LastSettlementDateTime, {{Year, Month, Day}, ?RANK_SETTLEMENT_TIME}) of
            -1 -> %%未结算
              true;
            _ -> false
          end;
        _ -> false
      end
  end.

%%结算，先结算世界排名
%%本服排行：10%， 20%，30%
%%好友排行：1名，2名，3名
settlement_server_ranking() ->
  case get_all_rank_info() of
    {success, RankInfoList} ->
      ServerCount = erlang:length(RankInfoList),
       PidList =
          lists:map(
            fun(Item) ->
              Pid = self(),
              RPid = spawn(fun() -> Pid !{self(), settlement_one_server_rank(Item, ServerCount)} end),
              {RPid, Item}
            end, RankInfoList),
       lists:map(
        fun({RPid, Item}) ->
          receive
            {RPid, Ret} ->
              case Ret of
                success ->
                  ?FILE_LOG_DEBUG("SETTELEMENT success, [~p]", [Item]);
                fail ->
                  ?FILE_LOG_DEBUG("SETTELEMENT fail, [~p]", [Item])
              end
          after
            10*1000 ->
              ?FILE_LOG_ERROR("SETTELEMENT server timeout", [])
          end
        end, PidList),
      ?FILE_LOG_DEBUG("settlement server finish", []);
    {fail, _Reason} ->
       ?FILE_LOG_ERROR("get all server rank info error", [])
  end.

settlement_one_server_rank({Uin, Point, Rank}, ServerCount) ->
  if
    ServerCount =< 0 ->
      ?FILE_LOG_ERROR("SERVER number is zero!", []),
      fail;
    true ->
      Percent = (Rank * 100) div ServerCount,
      %%所有人的奖励，并且清空上周的成绩
      {success, CacheNode} =  get_cache_node(Uin),
        case rpc:call(CacheNode, cache, settlement_server_rank_reward, [Uin, Percent, Rank, Point]) of
          success -> success;
          fail -> fail;
          Other ->
            ?FILE_LOG_DEBUG("settlement_server_reward error, reason = ~p", [Other]),
            fail
        end
  end.


settlement_friend_rank() ->
  case get_all_user() of
    {success, UinList} ->
      PidInfoList =
        lists:map(
          fun(Uin) ->
            Pid = self(),
            RPid = spawn(fun() -> Pid ! {self(), settlement_one_friend_rank(Uin)} end),
            {RPid, Uin}
          end, UinList),
      lists:map(
        fun({RPid, Uid}) ->
          receive
            {RPid, Ret} ->
              case Ret of
                success ->
                  ?FILE_LOG_DEBUG("SETTELEMENT success, [~p]", [Uid]);
                fail ->
                  ?FILE_LOG_DEBUG("SETTELEMENT fail, [~p]", [Uid])
              end
          after
            30*1000 ->
              ?FILE_LOG_ERROR("SETTELEMENT friend timeout", [])
          end
        end, PidInfoList),
      ?FILE_LOG_DEBUG("settlement server finish", []);
    fail ->
      ?FILE_LOG_DEBUG("GET ALL USER INFO ERROR", [])
  end.

settlement_one_friend_rank(Uin) ->
  case ranking:get_friend_endless_ranking(Uin) of
    {success, {_, ThisWeekRankList}} ->
      F = fun(Item, Id) -> Item#rank_info.uin =:= Id end,
      case cache_util:find_item_by_id(ThisWeekRankList, F, Uin) of
        {success, Item} ->
          {success, CacheNode} = get_cache_node(Uin),
          case rpc:call(CacheNode, cache, settlement_friend_rank_reward, [Uin, Item]) of
            success -> success;
            fail -> fail
          end;
        fail ->
          ?FILE_LOG_DEBUG("can not find user rank info from list, uin = ~p, List = ~p", [Uin, ThisWeekRankList]),
          fail
      end;
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("fail to get friend endless rank, reason= [~p, ~p]", [Uin, Reason]),
      fail
  end.

get_all_user() ->
  {success, DbNode} = dd_ms:read_config(database_node),
  case rpc:call(DbNode, database_monitor, execute, [get_all_user, 0]) of
    {success, UinList} -> {success, UinList};
    fail -> fail
  end.


get_cache_node(Uin) when is_integer(Uin)->
  {success, CacheHashRule} = dd_ms:read_config(cache_hash_rule),
  case hash_service_util:find_key_store_node(dd_util:to_list(Uin), CacheHashRule) of
    fail ->
      ?FILE_LOG_ERROR("no available data node, ~p", [Uin]),
      throw({custom, "HintSystemError"});
    {success, Node} when is_atom(Node) -> {success, Node}
  end.
