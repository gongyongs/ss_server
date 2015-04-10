%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 七月 2014 下午5:57
%%%-------------------------------------------------------------------
-module(gateway_util).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
-include("../dd_ms.hrl").
-include("../csv.hrl").
-include("../cache/cache_def.hrl").
-include("../ranking/ranking.hrl").
-include("../mail/mail.hrl").
-include("gateway.hrl").
%% API
-export([
  back_signature/1,
  signature_check/1,
  undefined_check/2,
  undefined_with_default_value/2,
  false_check/2,
  empty_check/2,
  get_cache_node/1,
  get_database_node/0,
  get_mail_node/0,
  get_uin_by_session/2,
  get_strength_remain_time/1,
  decode_super_team_json/1,
  decode_reward_team_json/1,
  decode_game_end_data/1,
  decode_strengthen_material/1,
  decode_sell_material/1,
  decode_add_data/1,
  decode_sync_data/1,
  decode_world_map_block_material/1,
  encode_json_notice/1,
  decode_tower_team_json/1
]).

-export([
  encode_json_mission/2,
  encode_json_shop/1
]).

-export([
  encode_friend_mail/1,
  encode_system_mail/1,
  encode_notice_mail/1
]).

-export([
  check_tollgate_mode/1,
  check_endless_tollgate/1,
  check_activity_tollgate/1
]).

-export([
  get_json_value/2,
  get_json_value_without_exception/3,
  get_plat_type/1
]).
undefined_check(Value, Reason) ->
  param_check(Value, undefined, Reason).

false_check(Value, Reason) ->
  param_check(Value, false, Reason).

empty_check(Value, Reason) ->
  param_check(Value, [], Reason).

undefined_with_default_value(Value, Default) ->
  undefined_with_default(Value, Default).

undefined_with_default(undefined, Default) -> Default;
undefined_with_default(Value, _) -> Value.

param_check(Value, Value, Reason) ->
  throw({custom, Reason});
param_check(Value, _, _) -> Value.


get_plat_type(PlatType) when is_list(PlatType) ->
  case PlatType of
    "" -> 0;                %%匿名登录
    "facebook" -> 1;       %%facebook平台登陆
    _ -> -1                  %%未知平台
  end;
get_plat_type(PlatType) -> PlatType.

get_cache_node(Uin)  ->
  {success, LoginHashRule} = get_config_param(cache_hash_rule),
  case hash_service_util:find_key_store_node(dd_util:to_list(Uin), LoginHashRule) of
    fail ->
      ?FILE_LOG_ERROR("no available data node, ~p", [Uin]),
      throw({custom, "HintSystemError"});
    {success, Node} when is_atom(Node) -> {success, Node}
  end.

get_config_param(Key) ->
  case dd_config:get_cfg(Key) of
    {success, Value} -> {success, Value};
    fail ->
      ?FILE_LOG_WARNING("get_key[~p] fail", [Key]),
      throw({custom, "HintSystemDataError"})
  end.

get_mail_node() ->
  {success, MailNode} = dd_ms:read_config(mail_node),
  {success, MailNode}.

get_database_node() ->
  {success, DBNode} = dd_ms:read_config(database_node),
  DBNode.
get_uin_by_session(Session, RequestData) ->
  {success, SessionNode} = dd_ms:read_config(session_node),
  case rpc:call(SessionNode, session_work, get_uin_by_session, [Session, node(), RequestData]) of
    {success, Uin} -> {success, Uin};
    fail -> throw({custom, "HintReLogin"});
    {badrpc, Reason} ->
      ?FILE_LOG_ERROR("get_uin_by_session error reason=~p", [Reason]),
      throw({custom, "HintSystemError"})
  end.

encode_friend_mail(AttachMailList) when is_list(AttachMailList) ->
  lists:foldr(
    fun({ID, {_Uin, PlayID, DisName}, Title, Content, Attach, Ts, Type}, TmpList) ->
      if
        Type =/= 1 -> TmpList;
        true ->
          [{
            struct,
            [
              {<<"mail_id">>, ID},
              {<<"source_id">>, dd_util:to_binary(PlayID)},
              {<<"source_name">>, dd_util:to_binary(DisName)},
              {<<"title">>, dd_util:to_binary(Content)},    %%客户端要求，标题显示内容，内容为标题，两者颠倒（2014-12-18）
              {<<"content">>, dd_util:to_binary(Title)},
              {<<"attach_type">>, Attach#mail_attachment.type},
              {<<"attach_id">>, dd_util:to_binary(Attach#mail_attachment.property_id)},
              {<<"attach_count">>, Attach#mail_attachment.count},
              {<<"mail_time">>, Ts}
            ]
          } | TmpList]
      end
    end, [], AttachMailList).

encode_system_mail(AttachMailList) ->
  lists:foldr(
    fun({ID, {_Uin, PlayID, DisName}, Title, Content, Attach, Ts, Type}, TmpList) ->
      ?FILE_LOG_DEBUG("all the mails are ~p",[AttachMailList]),
      if
        Type =/= 2 -> TmpList;
        true ->
          [{
            struct,
            [
              {<<"mail_id">>, ID},
              {<<"source_id">>, dd_util:to_binary(PlayID)},
              {<<"source_name">>, dd_util:to_binary(DisName)},
              {<<"title">>, dd_util:to_binary(Content)}, %%客户端要求，标题显示内容，内容为标题，两者颠倒（2014-12-18）
              {<<"content">>, dd_util:to_binary(Title)},
              {<<"attach_type">>, Attach#mail_attachment.type},
              {<<"attach_id">>, dd_util:to_binary(Attach#mail_attachment.property_id)},
              {<<"attach_count">>, Attach#mail_attachment.count},
              {<<"mail_time">>, Ts}
            ]
          } | TmpList]
      end
    end, [], AttachMailList).
encode_notice_mail(BulletinMailList) ->
  lists:map(
    fun({ID, Title, Content, Ts}) ->
      {
        struct,
        [
          {<<"mail_id">>, ID},
          {<<"title">>, dd_util:to_binary(Title)},
          {<<"content">>, dd_util:to_binary(Content)},
          {<<"mail_time">>, Ts}
        ]
      }
    end, BulletinMailList).


%%解密球员队伍数据
decode_super_team_json(Team) ->
  {struct, TeamJson} = mochijson2:decode(Team),
  ?FILE_LOG_DEBUG("now in decode_super_team_json TeamJson is ---->~p", [TeamJson]),
  TeamJsonList = get_json_value(<<"team">>, TeamJson),
  {_, TeamList, _} =
  lists:foldl(
    fun(ID, {Index, TmpList, TmpTree}) ->
			case ID =:= 0 of
				true->
					 {Index + 1, [{Index, dd_util:to_integer(ID)} | TmpList], TmpTree};
				_->
				      case gb_trees:lookup(ID, TmpTree) of
				        none ->
				          {Index + 1, [{Index, dd_util:to_integer(ID)} | TmpList], gb_trees:insert(ID, Index, TmpTree)};
				        {value, _} ->
				          ?FILE_LOG_ERROR("decode super team json error ,same tower id ", []),
				          throw({custom, "HintNetRequestDataError"})
				      end
			end
    end, {1, [], gb_trees:empty()}, TeamJsonList),
  TeamList.

%%解密赏金队伍数据
decode_reward_team_json(Team) ->
  {struct, TeamJson} = mochijson2:decode(Team),
  ?FILE_LOG_DEBUG("now in decode_reward_team_json TeamJson is ---->~p", [TeamJson]),
  TeamJsonList = get_json_value(<<"paramlist">>, TeamJson),
  {_, TeamList, _} =
  lists:foldl(
    fun(ID, {Index, TmpList, TmpTree}) ->
			case ID =:= 0 of
				true->
					 {Index + 1, [{Index, dd_util:to_integer(ID)} | TmpList], TmpTree};
				_->
				      case gb_trees:lookup(ID, TmpTree) of
				        none ->
				          {Index + 1, [{Index, dd_util:to_integer(ID)} | TmpList], gb_trees:insert(ID, Index, TmpTree)};
				        {value, _} ->
				          ?FILE_LOG_ERROR("decode reward team json error ,same tower id ", []),
				          throw({custom, "HintNetRequestDataError"})
				      end
			end
	end, {1, [], gb_trees:empty()}, TeamJsonList),
  TeamList.



decode_sync_data(Data) ->
  {struct, DataJson} = mochijson2:decode(Data),
  DataList = get_json_value(<<"data">>, DataJson),
  lists:map(
    fun({struct, Item}) ->
      Key = dd_util:to_list(get_json_value(<<"key">>, Item)),
      Value = get_json_value(<<"val">>, Item),
      {Key, Value}
    end, DataList).

decode_world_map_block_material(DataList) ->
  MaterialList = get_json_value(<<"data">>, DataList),
  {success, statistics_list(MaterialList)}.

%%纯粹测试借口
decode_add_data(DataList) ->
  AddGold =
    case proplists:get_value(<<"add_gold">>, DataList, undefined) of
      undefined -> 0;
      Value ->
        Gold = dd_util:to_integer(Value),
        if
          Gold < 0 -> 0;
          true ->
            Gold
        end
    end,
  AddGem =
    case proplists:get_value(<<"add_gem">>, DataList, undefined) of
      undefined -> 0;
      Value1 ->
        Gem = dd_util:to_integer(Value1),
        if
          Gem < 0 -> 0;
          true ->
            Gem
        end
    end,
  AddStrength =
    case proplists:get_value(<<"add_strength">>, DataList, undefined) of
      undefined -> 0;
      Value2 ->
        Strength = dd_util:to_integer(Value2),
        if
          Strength < 0 -> 0;
          true ->
            Strength
        end
    end,
  AddMaterialList  =
    case proplists:get_value(<<"add_material">>, DataList, undefined) of
      undefined -> [];
      [] -> [];
      List ->
        statistics_list(List)
    end,

  {AddMaterialList, AddGold, AddGem, AddStrength}.

decode_game_end_data(GameDataList) ->
  Result = get_json_value(<<"success">>, GameDataList),
  {struct, Data} = get_json_value(<<"data">>, GameDataList),
  %%TollgateID = dd_util:to_integer(get_json_value(<<"tollgate_id">>, Data)),
  NResult = dd_util:to_integer(Result),
  %%  if
  %%    TollgateID > 10000 andalso TollgateID < 20000 -> 0;
  %%    true -> dd_util:to_integer(Result)
  %%  end,
  decode_game_end_data(NResult, Data).


decode_game_end_data(0, DataList) ->       %%成功的数据解析
  TollgateID = dd_util:to_integer(get_json_value(<<"tollgate_id">>, DataList)),
  {GainStar, GainStarSeq} = decode_gain_star(dd_util:to_list(get_json_value(<<"gain_star">>, DataList))),
  GainScore = dd_util:to_integer(get_json_value(<<"gain_score">>, DataList)),
  GainDrop = get_pair(get_json_value(<<"gain_drop">>, DataList)),
  UseProps = get_pair(get_json_value(<<"use_props">>, DataList)),
  GainGold = dd_util:to_integer(get_json_value(<<"gain_gold">>, DataList)),
  EndlessCount = dd_util:to_integer(get_json_value_without_exception(<<"enddless_tollgate_count">>, DataList, 0)),
  JellyConsume = dd_util:to_integer(get_json_value_without_exception(<<"jelly_consume_count">>,DataList, 0)),
  DataStatistics = get_json_value_without_exception(<<"data_statistics">>,DataList, []),
  GameEndStatistics = decode_game_end_statistic(DataStatistics),
  {success, #game_end{success = 0, tollgate_id = TollgateID, endless_tollgate_num = EndlessCount, gain_star = GainStar, gain_star_seq = GainStarSeq, gain_gold = GainGold,
  gain_score = GainScore, gain_drops = GainDrop, use_props = UseProps, jelly_consume_num = JellyConsume, data_statistics = GameEndStatistics}};
decode_game_end_data(_, DataList) ->       %%失败的数据解析
  GainScore = dd_util:to_integer(get_json_value(<<"gain_score">>, DataList)),
  TollgateID = dd_util:to_integer(get_json_value(<<"tollgate_id">>, DataList)),
  UseProps = get_pair(get_json_value(<<"use_props">>, DataList)),
  JellyConsume = dd_util:to_integer(get_json_value_without_exception(<<"jelly_consume_count">>,DataList, 0)),
  DataStatistics = get_json_value_without_exception(<<"data_statistics">>, DataList, []),
  GameEndStatistics = decode_game_end_statistic(DataStatistics),
  {success, #game_end{success = -1, tollgate_id = TollgateID, endless_tollgate_num = 0, gain_star = 0, gain_star_seq = [-1, -1, -1], gain_gold = 0,
  gain_score = GainScore, gain_drops = [], use_props = UseProps, jelly_consume_num = JellyConsume, data_statistics = GameEndStatistics}}.

decode_game_end_statistic(StatisticLis) ->
  EmptyEndStatistics = #game_end_statistics{monster_list = [], total_build_tower = 0, total_kill_all_monster = 0, total_kill_boss_monster = 0,
  total_kill_normal_monster = 0, total_kill_obstacles = 0, total_use_skill = 0, total_kill_elite_monster = 0},
  lists:foldl(
    fun({struct, TowerItem}, TmpStatistics) ->
      TowerID = dd_util:to_list(get_json_value(<<"tower_id">>, TowerItem)),
      NormalMonster = get_pair(get_json_value(<<"normal_monster">>, TowerItem)),
      EliteMonster = get_pair(get_json_value(<<"elite_monster">>, TowerItem)),
      BossMonster = get_pair(get_json_value(<<"boss_monster">>, TowerItem)),
      UseSkill = dd_util:to_integer(get_json_value(<<"use_skill">>, TowerItem)),
      BuildCount = dd_util:to_integer(get_json_value(<<"build_count">>, TowerItem)),
      SellCount = dd_util:to_integer(get_json_value(<<"sell_count">>, TowerItem)),
      KillObstacle = dd_util:to_integer(get_json_value(<<"kill_obstacle">>, TowerItem)),
      TotalNormal = get_total_count(NormalMonster),
      TotalElite = get_total_count(EliteMonster),
      TotalBoss = get_total_count(BossMonster),
      KillMonsterStatistics = #kill_monster{normal_monster_list = NormalMonster, normal_monster_number = TotalNormal, elite_monster_list = EliteMonster, elite_monster_number = TotalElite, boss_monster_list = BossMonster, boss_monster_number = TotalBoss},
      StatisticsItem = #statistics_item{tower_id = TowerID, build_num = BuildCount, sell_num = SellCount, kill_obstacles = KillObstacle, use_skill_num = UseSkill, total_kill_monster = TotalNormal + TotalBoss + TotalElite, kill_monster = KillMonsterStatistics},
      update_game_end_statistics(TmpStatistics, StatisticsItem)
    end, EmptyEndStatistics, StatisticLis).

update_game_end_statistics(GameEndStatistics, StatisticsItem) when is_record(GameEndStatistics,game_end_statistics) andalso is_record(StatisticsItem, statistics_item) ->
  F = fun(Item, ID) -> Item#statistics_item.tower_id =:= ID end,
  case cache_util:find_item_by_id(GameEndStatistics#game_end_statistics.monster_list, F, StatisticsItem#statistics_item.tower_id) of
    {success, _} ->
      GameEndStatistics;
    fail ->
      GameEndStatistics#game_end_statistics{
        monster_list = [StatisticsItem | GameEndStatistics#game_end_statistics.monster_list],
        total_use_skill = GameEndStatistics#game_end_statistics.total_use_skill + StatisticsItem#statistics_item.use_skill_num,
        total_build_tower = GameEndStatistics#game_end_statistics.total_build_tower + StatisticsItem#statistics_item.build_num,
        total_kill_all_monster = GameEndStatistics#game_end_statistics.total_kill_all_monster + StatisticsItem#statistics_item.total_kill_monster,
        total_kill_normal_monster = GameEndStatistics#game_end_statistics.total_kill_normal_monster + StatisticsItem#statistics_item.kill_monster#kill_monster.normal_monster_number,
        total_kill_elite_monster = GameEndStatistics#game_end_statistics.total_kill_elite_monster + StatisticsItem#statistics_item.kill_monster#kill_monster.elite_monster_number,
        total_kill_boss_monster = GameEndStatistics#game_end_statistics.total_kill_boss_monster + StatisticsItem#statistics_item.kill_monster#kill_monster.boss_monster_number,
        total_kill_obstacles = GameEndStatistics#game_end_statistics.total_kill_obstacles + StatisticsItem#statistics_item.kill_obstacles
      }
  end.

%%[-1, 0, -1]
decode_gain_star(StarList) when is_list(StarList) ->
  Len = length(StarList),
  if
    Len =/= 3 ->
      ?FILE_LOG_ERROR("gain star data error, ~p", [StarList]),
      throw({custom, "HintRequestDataError"});
    true -> ok
  end,
  {StarNum, StarL} =
    lists:foldl(
      fun(Item, {TmpNum, TmpStarL}) ->
        case dd_util:to_integer(Item) of
          0 -> {TmpNum + 1, [0 | TmpStarL]};
          _ -> {TmpNum, [-1 | TmpStarL]}
        end
      end, {0, []}, StarList),
  {StarNum, lists:reverse(StarL)}.


get_json_value(Key, PropList) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      throw({custom, "HintSystemError"});
    Value -> Value
  end.

get_json_value_without_exception(Key, PropList, DefaultValue) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      DefaultValue;
    Value -> Value
  end.

get_pair([]) ->
  [];
get_pair(Lists) ->
  lists:map(
    fun({struct, Item}) ->
      Id = get_json_value(<<"id">>, Item),
      Count = get_json_value(<<"num">>, Item),
      {dd_util:to_list(Id), dd_util:to_integer(Count)}
    end, Lists).

%%{id, count}
get_total_count(List) ->
  lists:foldr(
    fun({_, Count}, TmpCount) ->
      TmpCount + Count
    end, 0, List).

%%{EquipIDList, MaterialList}
decode_strengthen_material(DataList) ->
  EquipList = get_json_value_without_exception(<<"equipment">>, DataList, []),
  MaterialList = get_json_value(<<"material">>, DataList),
  {lists:map(fun(EquipItem) -> dd_util:to_list(EquipItem) end, EquipList), statistics_list(MaterialList)}.

decode_sell_material(DataList) ->
  EquipmentList = get_json_value_without_exception(<<"equipment">>, DataList, []),
  MaterialList = get_json_value(<<"material">>, DataList),
  {lists:map(fun(EquipItem) -> dd_util:to_list(EquipItem) end, EquipmentList), statistics_list(MaterialList)}.

%%[] -> [{id, count}]
statistics_list(List) when is_list(List) ->
  Tree =
    lists:foldr(
      fun(ID, TmpTree) ->
        AtomId = list_to_atom(dd_util:to_list(ID)),
        case gb_trees:lookup(AtomId, TmpTree) of
          none -> gb_trees:insert(AtomId, 1, TmpTree);
          {value, Count} -> gb_trees:update(AtomId, Count + 1, TmpTree)
        end
      end, gb_trees:empty(), List),

  lists:map(
    fun({ID, Count}) ->
      {dd_util:to_list(ID), Count}
    end, gb_trees:to_list(Tree)).

%%%%%%%%%%json encode%$%%%%%%%%%%%%%%%%%

encode_json_shop(Shop) ->
  Shop_config =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"id">>, dd_util:to_binary(Item#goods.id)},
            {<<"remain_count">>, Item#goods.remain_count}
          ]
        }
      end, Shop#shop.goods_list),
  PermanentConfig =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"id">>, dd_util:to_binary(Item#goods.id)},
            {<<"remain_count">>, Item#goods.remain_count}
          ]
        }
      end, Shop#shop.permanent_buy_list),
  lists:merge(Shop_config, PermanentConfig).



encode_json_activity(PlayerActivity) when is_record(PlayerActivity, player_activity) ->
  ActivityList =
    lists:map(
      fun(ActivityRewardItem) ->
        Status =
          if
            ActivityRewardItem#activity_reward_item.finish_ts > 0 andalso ActivityRewardItem#activity_reward_item.get_reward_ts > 0 -> 3;
            ActivityRewardItem#activity_reward_item.finish_ts > 0 -> 2;
            true -> 1
          end,
        {
          struct,
          [
            {<<"id">>, ActivityRewardItem#activity_reward_item.id},
            {<<"status">>, Status}
          ]
        }
      end, PlayerActivity#player_activity.activity_reward_list),
    {
      struct,
      [
        {<<"activity_value">>, PlayerActivity#player_activity.activity_value},
        {<<"reward_list">>, ActivityList}
      ]
    }.

encode_json_mission_list(MissionList) when is_list(MissionList) ->
  lists:map(
    fun(MissionItem) ->
      Status =
        if
          MissionItem#mission_item.mission_finish_ts > 0 andalso MissionItem#mission_item.mission_get_reward_ts > 0 -> 3;
          MissionItem#mission_item.mission_finish_ts > 0 -> 2;
          true -> 1
        end,
      {
        struct,
        [
          {<<"id">>, dd_util:to_binary(MissionItem#mission_item.mission_id)},
          {<<"progress">>, MissionItem#mission_item.mission_progress_1},
          {<<"type">>, MissionItem#mission_item.mission_type},
          {<<"status">>, Status},
          {<<"finish_times">>, MissionItem#mission_item.mission_finish_times}
        ]
      }
    end, MissionList).

encode_json_mission(MissionList, PlayerActivity) when is_list(MissionList) andalso is_record(PlayerActivity, player_activity) ->
  {
    struct,
    [
      {<<"mission_list">>, encode_json_mission_list(MissionList)},
      {<<"activity">>, encode_json_activity(PlayerActivity)}
    ]
  }.

encode_json_notice(NoticeList) ->
  lists:map(
  fun(Notice)->
    {
      struct,
      [
        {<<"id">>, Notice#notice_rd.notice_id},
        {<<"title">>, dd_util:to_binary(Notice#notice_rd.notice_title)},
        {<<"date">>, dd_util:to_binary(Notice#notice_rd.notice_date)},
        {<<"detail">>, dd_util:to_binary(Notice#notice_rd.notice_detail)},
        {<<"pic_url">>, dd_util:to_binary(Notice#notice_rd.notice_pic_url)},
        {<<"sign">>, dd_util:to_binary(Notice#notice_rd.notice_sign)}
      ]
    }
  end,NoticeList).

decode_tower_team_json(Team) ->
  {struct, TeamJson} = mochijson2:decode(Team),
  TeamJsonList = get_json_value(<<"team">>, TeamJson),
  {_, TeamList, _} =
  lists:foldl(
    fun(ID, {Index, TmpList, TmpTree}) ->
      case gb_trees:lookup(ID, TmpTree) of
        none ->
          {Index + 1, [{Index, dd_util:to_list(ID)} | TmpList], gb_trees:insert(ID, Index, TmpTree)};
        {value, _} ->
          ?FILE_LOG_ERROR("decode tower team json error ,same tower id ", []),
          throw({custom, "HintNetRequestDataError"})
      end
    end, {1, [], gb_trees:empty()}, TeamJsonList),
  TeamList.

check_tollgate_mode(TollgateID) when is_integer(TollgateID) ->
  if
    TollgateID > 10000 -> throw({custom, "HintRequestDataError"});
    true -> ok
  end.

check_endless_tollgate(TollgateID) when is_integer(TollgateID) ->
  if
    TollgateID < 10000 -> throw({custom, "HintRequestDataError"});
    TollgateID > 20000 -> throw({custom, "HintRequestDataError"});
    true -> ok
  end.

check_activity_tollgate(TollgateID) when is_integer(TollgateID) ->
  if
    TollgateID < 20000 -> throw({custom, "HintRequestDataError"});
    true -> ok
  end.

get_strength_remain_time(Strength) ->
  cache_update_to_time:get_strength_cd(Strength).

back_signature(Data) ->
  dd_util:to_binary(dd_util:md5_string(dd_util:to_list(Data) ++ ?SIGNATURE)).

%%signature_check(_) -> ok.
signature_check(RequestDataList) ->
case proplists:get_value("sign", RequestDataList, undefined) of
   undefined -> success;
   Sign ->
     Encode = encode_signature(RequestDataList),
     MD5 = dd_util:md5_string(Encode),
     Signature = dd_util:to_list(Sign),
     ?FILE_LOG_DEBUG("encode = ~p, md5 = ~p, signature = ~p", [Encode, MD5, Signature]),
     if
       MD5 =:= Signature -> success;
       true ->
         throw({custom, "HintRequestDataError"})
     end
end.

encode_signature(RequestList) ->
  encode_signature_1(RequestList, "") ++ ?SIGNATURE.

encode_signature_1([], Encode) -> Encode;
encode_signature_1([{"sign", _Value} | _], Encode) -> Encode;
encode_signature_1([{Key, Value}, {"sign", _} | _], Encode) ->
  case dd_util:to_list(Key) of
    [] -> Encode;
    _ -> Encode ++ dd_util:to_list(Key) ++ "=" ++ dd_util:to_list(Value)
  end;
encode_signature_1([{Key, Value} | T], Encode) ->
  case dd_util:to_list(Key) of
    [] -> encode_signature_1(T, Encode);
    _ ->
      E = Encode ++ dd_util:to_list(Key) ++ "=" ++ dd_util:to_list(Value) ++ "&",
      encode_signature_1(T, E)
  end.


