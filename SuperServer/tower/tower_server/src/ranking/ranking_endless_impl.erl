%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 十二月 2014 下午4:02
%%%-------------------------------------------------------------------
-module(ranking_endless_impl).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
-include("../cache/cache_def.hrl").
-include("ranking.hrl").


%% API
-export([
  execute/2,
  ranking_endless/0,
  check_endless_settlement/2,
  check_endless_rank/1,
  settle_weekly_endless_rank/1
]).

execute(get_endless_rank, Uin) ->
  PlayerCount = dd_util:get_ets_size(endless_server_rank),
  TopTwentyList = ets:tab2list(endless_server_rank_top_20),
  case ets:lookup(endless_server_rank, Uin) of
    [] ->
      {success, {TopTwentyList, {0, 0, PlayerCount}}};
    [Rank] ->
      {success, {TopTwentyList, {Rank#rank_info.rank, Rank#rank_info.score, PlayerCount}}}
  end;

execute(update_player_basic_info, {Uin, UName, DisName}) ->      %%更新玩家的基本
  case ets:lookup(endless_server_rank, Uin) of
    [] -> success;
    [UserRank] ->
      NRank = UserRank#rank_info{uname = UName, dis_name = DisName},
      case ets:lookup(endless_server_rank_top_20, Uin) of
        [] -> success;
        [UserTopInfo] ->
          NUserTopInfo = UserTopInfo#rank_info{uname = UName, dis_name = DisName},  %%更新前二十
          ets:insert(endless_server_rank_top_20, NUserTopInfo)
      end,
      ets:insert(endless_server_rank, NRank),
      success
  end.


ranking_endless() ->
  SourceDataList = read_data_from_ets(),
  ?FILE_LOG_DEBUG("SOURCE LIST = ~p", [SourceDataList]),
  RankDataList = merge(SourceDataList),
  success = save_format_rank_data(RankDataList),
  ?FILE_LOG_DEBUG("ranking endless success, data = ~p", [RankDataList]),
  success.


save_format_rank_data(RandDataList) ->
  save_rank_data(RandDataList, 1).
save_rank_data([], _) -> success;
save_rank_data([H |T], Rank) ->
  NRankD = H#rank_info{rank = Rank},
  if
    Rank =< 20 ->
      ets:insert(endless_server_rank, NRankD),
      ets:insert(endless_server_rank_top_20, NRankD);
    true ->
      ets:insert(endless_server_rank, NRankD)
  end,
  save_rank_data(T, Rank + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%提取排名源数据,顺便排序
get_endless_score(List) ->
  OutList = get_score_1(List, []),
  sort_endless(OutList).
get_score_1([], OutList) -> OutList;
get_score_1([H | T], OutList) ->
  case H#ranking_tollgate_info.endless#endless_info.gain_max_ts of
    0 -> get_score_1(T, OutList);
    _ ->
      RankInfo = #rank_info{uin = H#ranking_tollgate_info.uin, uname = H#ranking_tollgate_info.uname, dis_name = H#ranking_tollgate_info.dis_name, rank = 0,
        score = H#ranking_tollgate_info.endless#endless_info.max_score, gain_score_ts = H#ranking_tollgate_info.endless#endless_info.gain_max_ts},
      get_score_1(T, [RankInfo | OutList])
  end.


sort_endless(List) ->
  SortFunc =
    fun(#rank_info{score = LPoint, gain_score_ts = LTs}, #rank_info{score = RPoint, gain_score_ts = RTs}) ->
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
  lists:sort(SortFunc, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(MAX_SINGLE_COUNT, 100).
%%遍历ets表, 获取所有玩家的数据
read_data_from_ets() ->
  {Result, C} = ets:match_object(ranking_tollgate, '$1', ?MAX_SINGLE_COUNT),
  read_data_1(C, [get_endless_score(Result)]).
read_data_1('$end_of_table', OutList) -> OutList;
read_data_1(Continue, OutList) ->
  {Result, Continue1} = ets:match_object(Continue),
  read_data_1(Continue1, [get_endless_score(Result)| OutList]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\
%%同时进行计算的进程数
-define(MAX_PROCESS_COUNT, 50).
%%采用多进程处理，并且保证接收每一个进程的结果是按顺序的
merge(InList) ->
  OutPidList = lists:reverse(merge_sort(InList, [], ?MAX_PROCESS_COUNT)),
  SortRetList =
    lists:foldl(
      fun(Pid, TmpRetList) ->
        receive
          {Pid, {sort_ret, SortRet}} ->
            [SortRet | TmpRetList]
        after
          30*1000 ->
            %%超时
            ?FILE_LOG_ERROR("sort ret timeout", []),
            TmpRetList
        end
      end, [], OutPidList),
  case SortRetList of
    [] -> throw({custom, "sort exception"});
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
merge_1([#rank_info{score = LV, gain_score_ts = LTS} = L | LT], [#rank_info{score = RV, gain_score_ts = RTS} = R | RT], OutList) when LV =:= RV ->
  if
    LTS =< RTS ->  merge_1(LT, [R |RT], [L | OutList]);
    true ->  merge_1([L | LT], RT, [R | OutList])
  end;
merge_1([#rank_info{score = LV} = L | LT], [#rank_info{score = RV} = R | RT], OutList) when LV > RV -> merge_1(LT, [R |RT], [L | OutList]);
merge_1([#rank_info{score = LV} = L | LT], [#rank_info{score = RV} = R | RT], OutList) when LV < RV -> merge_1([L | LT], RT, [R | OutList]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%每周一的凌晨三点结算上一周的成绩
-define(RANK_SETTLEMENT_DAY, 1).
-define(RANK_SETTLEMENT_TIME, {3, 0, 0}).

%%每周排名结算
settle_weekly_endless_rank(From) ->
  PlayerCount = dd_util:get_ets_size(endless_server_rank),
  PlayerRankList = get_all_rank_info(),
  settle_endless_server_rank(PlayerRankList, PlayerCount),
  settle_endless_friend_rank(PlayerRankList, PlayerCount),
  From ! settlement_finish.
%%%%%%%%%%%%%%
%%结算，先结算世界排名
%%本服排行：10%， 20%，30%
%%好友排行：1名，2名，3名
settle_endless_server_rank(PlayerRankList, PlayerCount) ->
  PidList = lists:map(
    fun(RankList) ->
      Pid = self(),
      spawn(fun() -> Pid !{self(), settle_endless_server_rank_1(RankList, PlayerCount, {0, 0})} end)
    end, PlayerRankList),
  lists:map(
    fun(RPid) ->
      receive
        {RPid, _} -> ok
      after
        2*60*1000 ->
          ?FILE_LOG_ERROR("settle server rank timeout", [])
      end
    end, PidList),
  ?FILE_LOG_DEBUG("settlement server rank finish", []).

%%发送奖励
settle_endless_server_rank_1([], _, {SuccessCnt, FailCnt}) ->
  ?FILE_LOG_ERROR("settle endless server rank success cnt = ~p, fail cnt = ~p", [SuccessCnt, FailCnt]);
settle_endless_server_rank_1([PlayerRank | T], PlayerCount, {SuccessCnt, FailCnt}) ->
  if
    PlayerCount =< 0 ->
      ?FILE_LOG_ERROR("player count is zero!", []),
      settle_endless_server_rank_1(T, PlayerCount, {SuccessCnt, FailCnt + 1});
    true ->
      Percent = (PlayerRank#rank_info.rank * 100) div PlayerCount,
      %%所有人的奖励，并且清空上周的成绩
      {success, CacheNode} =  get_cache_node(PlayerRank#rank_info.uin),
      case rpc:call(CacheNode, cache, settlement_server_rank_reward,
        [PlayerRank#rank_info.uin, Percent, PlayerRank#rank_info.rank, PlayerRank#rank_info.score]) of
        success ->
          settle_endless_server_rank_1(T, PlayerCount, {SuccessCnt + 1, FailCnt});
        fail ->
          settle_endless_server_rank_1(T, PlayerCount, {SuccessCnt, FailCnt + 1});
        Other ->
          ?FILE_LOG_DEBUG("settlement_server_reward error, reason = ~p", [Other]),
          settle_endless_server_rank_1(T, PlayerCount, {SuccessCnt, FailCnt + 1})
      end
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%好友排名发放奖励
settle_endless_friend_rank(PlayerRankList, PlayerCount) ->
  PidInfoList = lists:map(
    fun(RankList) ->
      Pid = self(),
      spawn(fun() -> Pid ! {self(), settle_endless_friend_rank_1(RankList, PlayerCount, {0, 0})} end)
    end, PlayerRankList),
  lists:map(
    fun(RPid) ->
      receive
        {RPid, _} -> ok
      after
        2*60*1000 ->
          ?FILE_LOG_ERROR("settle friend rank timeout", [])
      end
    end, PidInfoList),
  ?FILE_LOG_DEBUG("settlement server finish", []).

%%好友排名发放奖励
settle_endless_friend_rank_1([], _, {SuccessCnt, FailCnt}) ->
  ?FILE_LOG_DEBUG("settle endless friend rank, success cnt = ~p, fail cnt = ~p", [SuccessCnt, FailCnt]);
settle_endless_friend_rank_1([PlayerRank | T], PlayerCount, {SuccessCnt, FailCnt}) ->
  FriendUinList = query_player_friend(PlayerRank#rank_info.uin),
  case ranking_tollgate:get_endless_rank(PlayerRank#rank_info.uin, FriendUinList) of
    {success, RankInfo} ->
      {success, CacheNode} = get_cache_node(PlayerRank#rank_info.uin),
      case rpc:call(CacheNode, cache, settlement_friend_rank_reward, [PlayerRank#rank_info.uin, RankInfo]) of
        success ->
          settle_endless_friend_rank_1(T, PlayerCount, {SuccessCnt + 1, FailCnt});
        fail ->
          settle_endless_friend_rank_1(T, PlayerCount, {SuccessCnt, FailCnt + 1})
      end;
    no_rank ->
      ?FILE_LOG_DEBUG("get_endless_rank, no rank info, uin = ~p", [PlayerRank#rank_info.uin]),
      settle_endless_friend_rank_1(T, PlayerCount, {SuccessCnt, FailCnt + 1});
    Other ->
      ?FILE_LOG_ERROR("get endless rank error, reason = ~p, uin = ~p", [Other, PlayerRank#rank_info.uin]),
      settle_endless_friend_rank_1(T, PlayerCount, {SuccessCnt, FailCnt + 1})
  end.

%%查询好友的UinList
query_player_friend(Uin) ->
  {success, CacheNode} = get_cache_node(Uin),
  case rpc:call(CacheNode, cache, query_account, [Uin, ["platform_info"]]) of
    {success, [{"platform_info", PlatformInfo}]} ->
      lists:map(fun(Friend) -> Friend#friend_item.uin end, PlatformInfo#platform_info.player_friends);
    Other ->
      ?FILE_LOG_ERROR("query friend info error, uin = ~p ,reason = ~p", [Uin, Other]),
      []
  end.


%%获取表中的所有排名信息，且数据格式为，每一个列表元素为MAX_SINGLE_COUNT个排名数据
get_all_rank_info() ->
  {Result, C} = ets:match(endless_server_rank, '$1', ?MAX_SINGLE_COUNT),
  read_rank_info_1(C, Result).
read_rank_info_1('$end_of_table', OutList) -> OutList;
read_rank_info_1(Continue, OutList) ->
  {Result, Continue1} = ets:match(Continue),
  read_rank_info_1(Continue1, [Result | OutList]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_time(LastTs, SetTime) ->
  {Date, Time} = calendar:local_time(),
  %%是否到结算时间
  case dd_util:time_compare_by_datetime({Date, Time}, {Date, SetTime}) of
    1 -> %%达到结算时间,判断上一次结算时间
      LastSettlementDateTime = dd_util:to_local_time(dd_util:timestamp_to_datetime(LastTs)),
      case dd_util:time_compare_by_datetime(LastSettlementDateTime, {Date, SetTime}) of
        -1 -> true;
        _ ->  false
      end;
    _ -> false
  end.

%%检查每周无尽排名结算时间
check_endless_settlement(LastRankTs, LastSettleTs) ->
  {Date, _Time} = calendar:local_time(),
  WeekTh = calendar:day_of_the_week(Date),
  if
    WeekTh =:= ?RANK_SETTLEMENT_DAY ->    %%星期天
      case check_time(LastSettleTs, ?RANK_SETTLEMENT_TIME) of
        true ->
          %%应该发奖
          case check_time(LastRankTs, ?RANK_SETTLEMENT_TIME) of
            true ->  time_to_rank_and_settle;
            false ->  time_to_settle
          end;
        false -> not_the_time
      end;
    true -> not_the_time
  end.

%%检查无尽排名时间
check_endless_rank(LastRankTs) ->
  case check_time(LastRankTs, ?RANK_SETTLEMENT_TIME) of
    true -> time_to_rank;
    false -> not_the_time
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_cache_node(Uin) when is_integer(Uin)->
  {success, CacheHashRule} = dd_ms:read_config(cache_hash_rule),
  case hash_service_util:find_key_store_node(dd_util:to_list(Uin), CacheHashRule) of
    fail ->
      ?FILE_LOG_ERROR("no available data node, ~p", [Uin]),
      throw({custom, "HintSystemError"});
    {success, Node} when is_atom(Node) -> {success, Node}
  end.
