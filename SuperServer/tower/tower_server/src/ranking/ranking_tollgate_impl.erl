%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十二月 2014 下午7:42
%%%-------------------------------------------------------------------
-module(ranking_tollgate_impl).
-author("zqlt").

-include("../../deps/file_log/include/file_log.hrl").
-include("../cache/cache_def.hrl").
-include("ranking.hrl").

-define(DB_SINGLE_MAX_READ, 200).
%% API
-export([execute/2]).
-export([
  load_tollgate_data_from_db/0
]).

%%获取关卡的前三名（包括活动关卡，但不包括无尽关卡）
execute(get_friend_tollgate_rank, {Uin, FriendUinList, TollgateID}) ->
  ?FILE_LOG_DEBUG("ranking_tollgate_impl: get_friend_tollgate_info [~p, ~p, ~p]", [Uin, TollgateID, FriendUinList]),
  {success, FriendRankList} = get_tollgate_rank_data(Uin, FriendUinList, TollgateID),
  SortFunc =
    fun(#rank_info{score = LScore}, #rank_info{score = RScore}) ->
      if
        LScore > RScore -> true;
        true -> false
      end
    end,
  SortAfterList = lists:sort(SortFunc, FriendRankList),
  TopThree = lists:sublist(SortAfterList, 3),     %%只取前三名
  FormatList = format_rank(TopThree),
  {success, FormatList};

%%获取好友的无尽模式的得分，本周没有分数的则返回上周的分数
execute(get_friend_endless_rank, {Uin, FriendUinList}) ->
  ?FILE_LOG_DEBUG("ranking_tollgate_impl: get_friend_endless_ranking [~p]", [Uin]),
  {NScoreList, LastWeekList, ThisWeekList} =
    case catch get_endless_rank_data(Uin, FriendUinList) of
      {success,{NS, LW, TW}} -> {NS, LW, TW};
      _ -> {[], [], []}
    end,

  SortFunc =
    fun(#rank_info{score = LScore}, #rank_info{score = RScore}) ->
      if
        LScore > RScore -> true;
        true -> false
      end
    end,
  SortAfterLastWeekList = lists:sort(SortFunc, lists:merge(NScoreList, LastWeekList)),
  SortAfterThisWeekList = lists:sort(SortFunc, ThisWeekList),
  FormatLastWeekList = format_rank(SortAfterLastWeekList),
  FormatThisWeekList = format_rank(SortAfterThisWeekList),

  %%暂时对无成绩的放到上周成绩列表中
  {success, {[], FormatLastWeekList, FormatThisWeekList}};

execute(get_endless_rank, {Uin, FriendUinList}) ->
  ?FILE_LOG_DEBUG("ranking_tollgate_impl: get_endless_rank [~p]", [Uin]),
  {success, {_, ThisWeekList}} = get_endless_rank_data(Uin, FriendUinList),

  SortFunc =
    fun(#rank_info{score = LScore}, #rank_info{score = RScore}) ->
      if
        LScore > RScore -> true;
        true -> false
      end
    end,
  SortAfterThisWeekList = lists:sort(SortFunc, ThisWeekList),
  FormatThisWeekList = format_rank(SortAfterThisWeekList),
  F = fun(RankItem, ID) -> RankItem#rank_info.uin =:= ID end,
  case cache_util:find_item_by_id(FormatThisWeekList, F, Uin) of
    {success, Item} -> {success, Item};
    fail -> no_rank
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute(update_tollgate_score, {Uin, TollgateID, Score}) ->
  ?FILE_LOG_DEBUG("ranking_tollgate_impl: update_tollgate_score, uin = ~p, tollgateid = ~p, score = ~p", [Uin, TollgateID, Score]),
  case get_player_tollgate_info([Uin]) of
    [] ->
      {success, TollgateRankInfo} = query_tollgate_info(Uin),
      update_tollgate_score(TollgateRankInfo, TollgateID, Score),
      success;
    [PlayerTollgateInfo] ->
      update_tollgate_score(PlayerTollgateInfo, TollgateID, Score),
      success
  end;

execute(update_endless_score, {Uin, Score}) when is_integer(Uin) andalso is_integer(Score) andalso Score > 0 ->
  ?FILE_LOG_DEBUG("ranking_tollgate_impl: update_endless_score, uin = ~p, socre = ~p", [Uin, Score]),
  case get_player_endless_info([Uin]) of
    [] ->
      {success, TollgateRankInfo} = query_tollgate_info(Uin),
      update_endless_score(TollgateRankInfo, Score),
      success;
    [PlayerEndlessInfo] ->
      update_endless_score(PlayerEndlessInfo, Score),
      success
  end;

execute(update_player_basic_info, {Uin, UName, DisName}) when is_list(DisName) andalso is_integer(Uin) andalso is_list(UName) ->
  case ets:lookup(ranking_tollgate, Uin) of
    [] -> success;
    [UserTollgateData] ->
      NUserTollgate = UserTollgateData#ranking_tollgate_info{uname = UName, dis_name = DisName},
      ranking_endless:update_player_basic_info(Uin, {UName, DisName}),
      ets:insert(ranking_tollgate, NUserTollgate),
      success
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%根据好友列表和关卡值获取好友关卡数据
get_tollgate_rank_data(Uin, FriendUinList, TollgateID) ->
  if
    TollgateID > 10000 andalso TollgateID < 20000 ->
      ?FILE_LOG_ERROR("get_tollgate_data error, tollgate id = ~p", [TollgateID]),
      throw({custom, "HintSystemDataError"});
    true -> ok
  end,
  case FriendUinList of
    [] -> %%需要把自己的数据提取出去
      case get_player_tollgate_info([Uin]) of
        [] -> {success, []};
        [SelfData] ->
          case get_tollgate_score(SelfData, TollgateID) of
            not_exist -> {success, []};
            {success, RankInfo} -> {success, [RankInfo]}
          end
      end;
    [-1] ->  %%获取所有玩家数据
      {success, get_all_player_tollgate_data(TollgateID)};
    _ ->     %%好友数据
      UserDataList = get_player_tollgate_info([Uin | FriendUinList]),
      {success, split_tollgate_data(UserDataList, TollgateID, [])}
  end.

get_tollgate_score(RankTollgateInfo, TollgateID) when is_record(RankTollgateInfo, ranking_tollgate_info) andalso is_integer(TollgateID) ->
  case gb_trees:lookup(TollgateID, RankTollgateInfo#ranking_tollgate_info.tg_tree) of
    none -> not_exist;
    {value, RankTg} ->
      RankInfo = #rank_info{uin = RankTollgateInfo#ranking_tollgate_info.uin, uname = RankTollgateInfo#ranking_tollgate_info.uname,
        dis_name = RankTollgateInfo#ranking_tollgate_info.dis_name, rank = 0, score = RankTg#rank_tg.tg_max_score, gain_score_ts = RankTg#rank_tg.tg_gain_ts},
      {success, RankInfo}
  end.

update_tollgate_score(_, _, 0) -> ok;
update_tollgate_score(RankTollgateInfo, TollgateID, Score) when is_record(RankTollgateInfo, ranking_tollgate_info) andalso is_integer(TollgateID) andalso is_integer(Score) andalso Score > 0 ->
  case gb_trees:lookup(TollgateID, RankTollgateInfo#ranking_tollgate_info.tg_tree) of
    none ->
      Type =
        if
          TollgateID < 10000 andalso TollgateID > 0 -> 1;
          TollgateID > 20000 andalso TollgateID < 30000 -> 3;
          true ->
            throw({custom, "TollgateIDError"})
        end,
      TG = #rank_tg{tg_id = TollgateID, tg_max_score = Score, tg_type = Type, tg_gain_ts = 0},
      NTree = gb_trees:insert(TollgateID, TG, RankTollgateInfo#ranking_tollgate_info.tg_tree),
      Ret = ets:update_element(ranking_tollgate, RankTollgateInfo#ranking_tollgate_info.uin, {#ranking_tollgate_info.tg_tree, gb_trees:balance(NTree)}),
      ?FILE_LOG_DEBUG("update tollgate score => ets update element = ~p", [Ret]);
    {value, RankTg} ->
      if
        Score =< RankTg#rank_tg.tg_max_score -> ok;
        true ->
          NRankTg = RankTg#rank_tg{tg_max_score = Score},
          NRTree = gb_trees:update(TollgateID, NRankTg, RankTollgateInfo#ranking_tollgate_info.tg_tree),
          Ret1 = ets:update_element(ranking_tollgate, RankTollgateInfo#ranking_tollgate_info.uin, {#ranking_tollgate_info.tg_tree, gb_trees:balance(NRTree)}),
          ?FILE_LOG_DEBUG("update tollgate score => ets update element = ~p", [Ret1])
      end
  end.

get_player_tollgate_info(UinList) ->
  lists:foldl(
    fun(Uin, TmpList) ->
      case ets:lookup(ranking_tollgate, Uin) of
        [] -> TmpList;
        [UserTollgateData] -> [UserTollgateData | TmpList]
      end
    end, [], UinList).

get_all_player_tollgate_data(TollgateId) ->
  List = traverse_ets(),
  ?FILE_LOG_DEBUG("list = ~p, len = ~p", [List, length(List)]),
  split_tollgate_data(List, TollgateId, []).

split_tollgate_data([], _, OutList) -> OutList;
split_tollgate_data([H | T], TollgateID, OutList) ->
  case get_tollgate_score(H, TollgateID) of
    not_exist -> split_tollgate_data(T, TollgateID, OutList);
    {success, RankInfo} -> split_tollgate_data(T, TollgateID, [RankInfo | OutList])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%根据好友列表和关卡值获取好友关卡数据
%%返回{LastWeekList， ThisWeekList}
get_endless_rank_data(Uin, FriendUinList) ->
  case FriendUinList of
    [] -> %%需要把自己的数据提取出去
      case get_player_endless_info([Uin]) of
        [] ->
          {success, TollgateInfo} = query_tollgate_info(Uin),
          {success, get_endless_score(TollgateInfo)};
        [SelfData] -> {success, get_endless_score(SelfData)}
      end;
    [-1] ->  %%获取所有玩家数据
      {success, get_all_player_endless_data()};
    _ ->     %%好友数据
      UserDataList = get_player_endless_info([Uin | FriendUinList]),
      {success, split_endless_data(UserDataList, {[], [], []})}
  end.

%%获取无尽数据
get_endless_score(RankTollgateInfo) when is_record(RankTollgateInfo, ranking_tollgate_info) ->
  ThisWeekScore = RankTollgateInfo#ranking_tollgate_info.endless#endless_info.this_score,
  ThisWeekGainTs = RankTollgateInfo#ranking_tollgate_info.endless#endless_info.gain_this_ts,
  LastWeekScore = RankTollgateInfo#ranking_tollgate_info.endless#endless_info.last_score,
  LastWeekGainTs = RankTollgateInfo#ranking_tollgate_info.endless#endless_info.gain_last_ts,
  RankInfo = #rank_info{uin = RankTollgateInfo#ranking_tollgate_info.uin, uname = RankTollgateInfo#ranking_tollgate_info.uname,
    dis_name = RankTollgateInfo#ranking_tollgate_info.dis_name, rank = 0, score = 0},
  if
    ThisWeekGainTs =< 0 andalso LastWeekGainTs =< 0 -> {[RankInfo#rank_info{score = 0, gain_score_ts = 0}], [], []};
    ThisWeekGainTs =< 0 -> {[], [RankInfo#rank_info{score = LastWeekScore, gain_score_ts = LastWeekGainTs}], []};
    true -> {[], [], [RankInfo#rank_info{score = ThisWeekScore, gain_score_ts = ThisWeekGainTs}]}
  end.

%%更新无尽数据
update_endless_score(RankTollgateInfo, Score) when is_record(RankTollgateInfo, ranking_tollgate_info) andalso is_integer(Score) andalso Score > 0->
  if
    Score =< RankTollgateInfo#ranking_tollgate_info.endless#endless_info.this_score -> ok;
    true ->
      NEndlessInfo =
        if
          Score =< RankTollgateInfo#ranking_tollgate_info.endless#endless_info.max_score ->
            RankTollgateInfo#ranking_tollgate_info.endless#endless_info{this_score = Score, gain_this_ts = dd_util:timestamp()};
          true ->
            RankTollgateInfo#ranking_tollgate_info.endless#endless_info{this_score = Score, gain_this_ts = dd_util:timestamp(), max_score = Score, gain_max_ts = dd_util:timestamp()}
        end,
      Ret = ets:update_element(ranking_tollgate, RankTollgateInfo#ranking_tollgate_info.uin, {#ranking_tollgate_info.endless, NEndlessInfo}),
      ?FILE_LOG_DEBUG("update endless score => score = ~p, ets update element ret = ~p", [Score, Ret])
  end.

%%获取玩家的关卡等缓存数据
get_player_endless_info(UinList) ->
  lists:foldl(
    fun(Uin, TmpList) ->
      case ets:lookup(ranking_tollgate, Uin) of
        [] -> TmpList;
        [UserTollgateData] -> [UserTollgateData | TmpList]
      end
    end, [], UinList).

get_all_player_endless_data() ->
  List = traverse_ets(),
  split_endless_data(List, {[], [], []}).

split_endless_data([], {NoCore, LastWeek, ThisWeek}) -> {NoCore, LastWeek, ThisWeek};
split_endless_data([H | T], {NoCore, LastWeek, ThisWeek}) ->
  {NS, LW, TW} = get_endless_score(H),
  split_endless_data(T, {lists:merge(NoCore, NS), lists:merge(LastWeek, LW), lists:merge(ThisWeek, TW)}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(MAX_SINGLE_COUNT, 50).
%%遍历ets表, 获取所有玩家的数据
traverse_ets() ->
  {Result, C} = ets:match_object(ranking_tollgate, '$1', ?MAX_SINGLE_COUNT),
  traverse_list_1(C, Result).
traverse_list_1('$end_of_table', OutList) -> OutList;
traverse_list_1(Continue, OutList) ->
  {Result, Continue1} = ets:match_object(Continue),
  traverse_list_1(Continue1, lists:merge(OutList, Result)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%从缓存中调取玩家的数据
query_tollgate_info(Uin) when is_integer(Uin) ->
  {success, CacheNode} = get_cache_node(Uin),
  case rpc:call(CacheNode, cache, query_account, [Uin, ["stage", "platform_info"]]) of
    {success, InfoList} ->
      Stage =
        case proplists:get_value("stage", InfoList, undefined) of
          undefined ->
            ?FILE_LOG_ERROR("query tollgate info error, uin = ~p, stage info not exist", [Uin]),
            throw({custom, "HintSystemError"});
          SValue -> SValue
        end,
      PlatformInfo =
        case proplists:get_value("platform_info", InfoList, undefined) of
          undefined ->
            ?FILE_LOG_ERROR("query tollgate info error, uin = ~p, platform_info not exist", [Uin]),
            throw({custom, "HintSystemError"});
          PValue -> PValue
        end,
      TollgateInfo = to_ranking_tollgate(Uin, PlatformInfo, Stage),
      ets:insert(ranking_tollgate, TollgateInfo),
      {success, TollgateInfo};
    {fail, Reason} ->
      ?FILE_LOG_ERROR("query tollgate info error, uin = ~p, reason = ~p", [Reason]),
      throw({custom, "HintSystemError"})
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format_rank(SortList) ->
  format_rank_1(SortList, 1, []).
format_rank_1([], _, OutList) -> lists:reverse(OutList);
format_rank_1([H | T], Rank, OutList) ->
  format_rank_1(T, Rank + 1, [H#rank_info{rank = Rank} | OutList]).

get_cache_node(Uin) ->
  {success, CacheHashRule} = dd_ms:read_config(cache_hash_rule),
  case hash_service_util:find_key_store_node(dd_util:to_list(Uin), CacheHashRule) of
    fail ->
      ?FILE_LOG_ERROR("no available data node, ~p", [Uin]),
      throw({custom, "HintSystemError"});
    {success, Node} when is_atom(Node) -> {success, Node}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_tollgate_data_from_db() ->
  ets:delete_all_objects(ranking_tollgate),
  case read_db(0, ?DB_SINGLE_MAX_READ, []) of
    {success, DataList} ->
      insert_data_to_ets(DataList),
      success;
    fail -> fail
  end.

insert_data_to_ets(DataList) ->
  lists:map(
    fun(RankTollgateInfo) ->
      ets:insert(ranking_tollgate, RankTollgateInfo)
    end, DataList),
  success.

read_db(Index, Len, TollgateList) ->
  {success, DbNode} = dd_ms:read_config(database_node),
  case rpc:call(DbNode, database_monitor, execute, [get_user_stage_info, {Index, Len}]) of
    {success, DataList} ->
      L = lists:map(
        fun({Uin, Platform, Stage}) ->
          to_ranking_tollgate(Uin, Platform, Stage)
        end, DataList),
      Length = length(L),
      if
        Length < Len -> {success, lists:merge(TollgateList, L)};
        true -> read_db(Index + Len, Len, lists:merge(TollgateList, L))
      end;
    fail ->
      ?FILE_LOG_ERROR("read_to_db_ets fail", []),
      fail;
    Other ->
      ?FILE_LOG_ERROR("read_to_db_ets fail ,reason = ~p", [Other]),
      fail
  end.

to_ranking_tollgate(Uin, Platform, Stage) ->
  BasicTree =
    lists:foldl(
      fun(Tollgate, TmpTree) ->
        case gb_trees:lookup(Tollgate#tollgate.id, TmpTree) of
          none ->
            V = #rank_tg{tg_id = Tollgate#tollgate.id, tg_max_score = Tollgate#tollgate.max_score, tg_type = 1, tg_gain_ts = 0},
            gb_trees:insert(Tollgate#tollgate.id, V, TmpTree);
          {value, TG} ->
            VTg = TG#rank_tg{tg_id = Tollgate#tollgate.id, tg_max_score = Tollgate#tollgate.max_score, tg_type = 1, tg_gain_ts = 0},
            gb_trees:update(Tollgate#tollgate.id, VTg, TmpTree)
        end
      end, gb_trees:empty(), Stage#stage.base_tollgate_list),
  Tree =
    lists:foldl(
      fun(AcTollgate, TmpTree) ->
        case gb_trees:lookup(AcTollgate#activity_tollgate_item.tollgate_id, TmpTree) of
          none ->
            AcV = #rank_tg{tg_id = AcTollgate#activity_tollgate_item.tollgate_id, tg_max_score = AcTollgate#activity_tollgate_item.max_score, tg_type = 3, tg_gain_ts = 0},
            gb_trees:insert(AcTollgate#activity_tollgate_item.tollgate_id, AcV, TmpTree);
          {value, AcTG} ->
            AcVTg = AcTG#rank_tg{tg_id = AcTollgate#activity_tollgate_item.tollgate_id, tg_max_score = AcTollgate#activity_tollgate_item.max_score, tg_type = 3, tg_gain_ts = 0},
            gb_trees:update(AcTollgate#activity_tollgate_item.tollgate_id, AcVTg, TmpTree)
        end
      end, BasicTree, Stage#stage.ac_tollgate),

  Endless = #endless_info{
    max_score = Stage#stage.endless_tollgate#endless_tollgate.max_score,
    gain_max_ts = Stage#stage.endless_tollgate#endless_tollgate.max_score_gain_ts,
    this_score = Stage#stage.endless_tollgate#endless_tollgate.this_week_max_score,
    gain_this_ts = Stage#stage.endless_tollgate#endless_tollgate.this_week_max_score_gain_ts,
    last_score = Stage#stage.endless_tollgate#endless_tollgate.last_week_max_score,
    gain_last_ts = Stage#stage.endless_tollgate#endless_tollgate.last_week_max_score_gain_ts
  },
  #ranking_tollgate_info{
    uin = Uin,
    dis_name = Platform#platform_info.player_dis_name,
    uname = Platform#platform_info.player_id,
    tg_tree = Tree,
    endless = Endless
  }.








