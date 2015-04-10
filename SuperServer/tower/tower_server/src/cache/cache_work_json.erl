%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 一月 2015 下午5:25
%%%-------------------------------------------------------------------
-module(cache_work_json).
-author("zqlt").
-include("cache_def.hrl").
-include("../ranking/ranking.hrl").

%% API
-export([
  encode_json_team/1,
  encode_json_change_team/1,
  encode_json_player/1,
  encode_json_login_reward/1,
  encode_json_tower/1,
  encode_json_tollgate/1,
  encode_json_backpack/1,
  encode_json_mission/2,
  encode_json_achievement_list/1,
  encode_json_mission_list/1,
  encode_json_shop/1,
  encode_json_guide_step/1,
  encode_json_lottery/1,
  encode_json_lottery_list/1,
  encode_json_endless_drop/1,
  encode_json_tollgate_drop/1,
  encode_json_activity_tollgate/2,
  encode_json_tollgate_mode/2,
  encode_json_endless_tollgate/2,
  encode_json_cdkey_package/1,
  encode_json_basic_info/1,
  encode_json_friend_endless_rank/1,
  encode_json_server_endless_rank/2,
  encode_json_tower_item/1,
  encode_json_equipment_item/1,
  encode_json_activity/1,
  encode_json_treasure_list/1,
  encode_json_select_tower/1,
  encode_json_tollgate_sweep_drop/1,
  encode_json_reward_match/1
]).

%%针对对外接口返回数据的格式化接口
%%所有返回的数据均格式化为json且二进制化

%%球员队伍信息
encode_json_team(SuperTeam)->
  Select =
    lists:map(
      fun({Pos, ID}) ->
        {
          struct,
          [
            {<<"pos">>, Pos},
            {<<"id">>, ID}
          ]
        }
      end, SuperTeam#players.select_players),
  {
    struct,
    [
      {<<"select">>, Select}
    ]
  }.

%%change team接口返回数据打包
encode_json_change_team(SuperTeam)->
  Select =
    lists:map(
      fun({Pos, ID}) ->
        {
          struct,
          [
            {<<"pos">>, Pos},
            {<<"id">>, ID}
          ]
        }
      end, SuperTeam),
  {
    struct,
    [
      {<<"select">>, Select}
    ]
  }.

%%玩家基本数据
encode_json_player(PlayerBasicData) ->
  {PlayerID, PlayerName, Gold, Gem, StrengthV, StrengthCD,RM_Ticket,RM_Ticket_CD} = PlayerBasicData,
  {
    struct,
    [
      {<<"uname">>, dd_util:to_binary(PlayerID)},
      {<<"dis_name">>, dd_util:to_binary(PlayerName)},
      {<<"gold">>, dd_util:to_integer(Gold)},
      {<<"gem">>, dd_util:to_integer(Gem)},
      {<<"strength">>, dd_util:to_integer(StrengthV)},
      {<<"strength_cd">>, StrengthCD},
	  {<<"rm_ticket">>, dd_util:to_integer(RM_Ticket)},
	  {<<"rm_ticket_cd">>, RM_Ticket_CD}
    ]
  }.

%%登陆奖励数据
encode_json_login_reward(LoginReward) when is_record(LoginReward, login_reward) ->
  RewardList =
    lists:map(
      fun(Item) ->
        Item#login_reward_item.id
      end, LoginReward#login_reward.login_reward_list),
  {
    struct,
    [
      {<<"login_times">>, LoginReward#login_reward.login_times},
      {<<"reward_list">>, RewardList}
    ]
  }.

%%防御塔装备数据
encode_json_equipment_item(Equipment) when is_record(Equipment, equipment) ->
  {
    struct,
    [
      {<<"id">>, dd_util:to_binary(Equipment#equipment.id)},
      {<<"no">>, dd_util:to_binary(Equipment#equipment.no)},
      {<<"exp">>, Equipment#equipment.exp}
    ]
  }.

%%防御塔数据
encode_json_tower_item(Tower) when is_record(Tower, character) ->
  EquipL =
    lists:map(
      fun(EquipItem) ->
        encode_json_equipment_item(EquipItem)
      end, Tower#character.equipment_list),
  InscriptionL =
    lists:map(
      fun(Ins) ->
        dd_util:to_binary(Ins#inscription.id)
      end, Tower#character.inscription_list),
  {
    struct,
    [
      {<<"id">>, dd_util:to_binary(Tower#character.id)},
      {<<"equipment">>, EquipL},
      {<<"inscription">>, InscriptionL}
    ]
  }.
%%格式化玩家所有塔的数据以及选中塔
encode_json_tower(Tower) when is_record(Tower, heros)->
  Select =
    lists:map(
      fun({Pos, ID}) ->
        {
          struct,
          [
            {<<"pos">>, Pos},
            {<<"id">>, dd_util:to_binary(ID)}
          ]
        }
      end, Tower#heros.select_hero),
  L =
    lists:map(
      fun(HeroItem) ->
        encode_json_tower_item(HeroItem)
      end, Tower#heros.character_lists),
  {
    struct,
    [
      {<<"hero">>, L},
      {<<"select">>, Select}
    ]
  }.

encode_json_select_tower(Tower) when is_record(Tower, heros) ->
  SFun =
    fun({P1, _}, {P2, _}) ->
      if
        P1 < P2 -> true;
        true -> false
      end
    end,

  Select = lists:sort(SFun, Tower#heros.select_hero),
  lists:map(
    fun({_, ID}) ->
      case cache_util:find_tower_by_id(Tower#heros.character_lists, ID) of
        {success, Item} -> encode_json_tower_item(Item);
        fail -> throw({custom, "HintSystemDataError"})
      end
    end, Select).

%%格式化时间地图采集物
encode_json_map_block_item(BlockItem) when is_record(BlockItem, harvest_obstacles_item) ->
  {
    struct,
    [
      {<<"id">>, dd_util:to_binary(BlockItem#harvest_obstacles_item.id)},
      {<<"remain_number">>, BlockItem#harvest_obstacles_item.remain_number}
    ]
  }.
%%格式化关卡
encode_json_tollgate(Stage) when is_record(Stage, stage) ->
  BaseTollgateJsonList =
    lists:map(
      fun(Tollgate) ->
        {
          struct,
          [
            {<<"id">>, Tollgate#tollgate.id},
            {<<"max_score">>, Tollgate#tollgate.max_score},
            {<<"max_star">>, Tollgate#tollgate.max_star},
            {<<"star_seq">>, Tollgate#tollgate.max_star_seq},
            {<<"remain_times">>, Tollgate#tollgate.daily_remain_times},
            {<<"addition_times">>, Tollgate#tollgate.addition_remain_times}
          ]
        }
      end, Stage#stage.base_tollgate_list),
  HarvestBlock =
    lists:map(
      fun(HarvestItem) ->
        encode_json_map_block_item(HarvestItem)
      end, Stage#stage.harvest_obstacles_list),
  EndlessTollgate =
    {
      struct,
      [
        {<<"max_score">>, Stage#stage.endless_tollgate#endless_tollgate.max_score},
        {<<"last_week_max_score">>, Stage#stage.endless_tollgate#endless_tollgate.last_week_max_score},
        {<<"this_week_max_score">>, Stage#stage.endless_tollgate#endless_tollgate.this_week_max_score}
      ]
    },
  ActivityList =
    lists:map(
      fun(ActivityTollgate) ->
        {
          struct,
          [
            {<<"id">>, ActivityTollgate#activity_tollgate_item.tollgate_id},
            {<<"max_score">>, ActivityTollgate#activity_tollgate_item.max_score},
            {<<"max_star">>, ActivityTollgate#activity_tollgate_item.max_star},
            {<<"star_seq">>, ActivityTollgate#activity_tollgate_item.max_star_seq},
            {<<"remain_times">>, ActivityTollgate#activity_tollgate_item.daily_remain_times}
          ]
        }
      end, Stage#stage.ac_tollgate),
  {
    struct,
    [
      {<<"base_tollgate">>, BaseTollgateJsonList},
      {<<"activity_tollgate">>, ActivityList},
      {<<"endless_tollgate">>, EndlessTollgate},
      {<<"map_block_list">>, HarvestBlock}
    ]
  }.

%%格式化背包数据
encode_json_backpack(BackPack) when is_record(BackPack, backpack)->
  S_EquipL =
    lists:map(
      fun({ID, {UUID,Count}}) ->
        {
          struct,
          [
			{<<"uuid">>, dd_util:to_binary(UUID)},
            {<<"id">>, ID},
            {<<"count">>, Count}
          ]
        }
      end, gb_trees:to_list(BackPack#backpack.s_equipment_list)),
  S_MaterialL =
    lists:map(
      fun({ID, {UUID,Count}}) ->
        {
          struct,
          [
			{<<"uuid">>, dd_util:to_binary(UUID)},
            {<<"id">>, ID},
            {<<"count">>, Count}
          ]
        }
      end, gb_trees:to_list(BackPack#backpack.s_material_list)),
  S_FragmentL =
    lists:map(
      fun({ID, {UUID,Count}}) ->
        {
          struct,
          [
			{<<"uuid">>, dd_util:to_binary(UUID)},
            {<<"id">>, ID},
            {<<"count">>, Count}
          ]
        }
      end, gb_trees:to_list(BackPack#backpack.s_fragment_list)),
  S_ConsumablesL =
    lists:map(
      fun({ID, {UUID,Count}}) ->
        {
          struct,
          [
			{<<"uuid">>, dd_util:to_binary(UUID)},
            {<<"id">>, ID},
            {<<"count">>, Count}
          ]
        }
      end, gb_trees:to_list(BackPack#backpack.s_consumables_list)),
  S_CardL =
    lists:map(
      fun({ID, {UUID,Count}}) ->
        {
          struct,
          [
			{<<"uuid">>, dd_util:to_binary(UUID)},
            {<<"id">>, ID},
            {<<"count">>, Count}
          ]
        }
      end, gb_trees:to_list(BackPack#backpack.s_card_list)),
  {
    struct,
    [
	 	%superstar属性
		{<<"s_equipment">>, S_EquipL},
	  	{<<"s_material">>, S_MaterialL},
		{<<"s_fragment">>, S_FragmentL},
		{<<"s_consumables">>, S_ConsumablesL},
	  	{<<"s_card">>, S_CardL}
    ]
  }.

%%格式化赏金赛数据
encode_json_reward_match(Reward_match) when is_record(Reward_match, reward_match)->
  CardList =
    lists:map(
      fun(Rm_card) ->
        {
          struct,
          [
            {<<"index">>, Rm_card#rm_card.index},
			{<<"id">>, Rm_card#rm_card.id},
			{<<"type">>, Rm_card#rm_card.type}
          ]
        }
      end, Reward_match#reward_match.card_list),
  CardHidList =
    lists:map(
      fun(Rm_card) ->
        {
          struct,
          [
            {<<"index">>, Rm_card#rm_card.index},
			{<<"id">>, Rm_card#rm_card.id},
			{<<"type">>, Rm_card#rm_card.type}
          ]
        }
      end, Reward_match#reward_match.card_list_hid),
  BuffList =
	lists:map(
      fun(Rm_buff) ->
        {
          struct,
          [
		   	{<<"index">>, Rm_buff#rm_buff.index},
			{<<"id">>, Rm_buff#rm_buff.id},
			{<<"type">>, Rm_buff#rm_buff.type}
          ]
        }
      end, Reward_match#reward_match.buff_list),
  ComboList =
	lists:map(
      fun(Rm_combo) ->
        {
          struct,
          [
			{<<"level">>, Rm_combo#rm_combo.level},
			{<<"max">>, Rm_combo#rm_combo.max}
          ]
        }
      end, Reward_match#reward_match.combomax),
  LevelCountList =
	lists:map(
      fun(Rm_combo) ->
        {
          struct,
          [
			{<<"level">>, Rm_combo#rm_combo.level},
			{<<"max">>, Rm_combo#rm_combo.max}
          ]
        }
      end, Reward_match#reward_match.levelcount),
	RewardTeam = 
	  lists:map(fun({Pos,PlayerId})->
				{
					struct,
			          [
			            {<<"pos">>, Pos},
			            {<<"id">>, PlayerId}
			          ]	 
				}
				end,Reward_match#reward_match.reward_team),
	{
		struct,
	    [
		{<<"fee">>, Reward_match#reward_match.fee},
		{<<"fee_temp">>, Reward_match#reward_match.fee_temp},
		{<<"fee_battle">>, Reward_match#reward_match.fee_battle},
		{<<"status">>, Reward_match#reward_match.status},
		{<<"battle_id">>, Reward_match#reward_match.battle_id},
		{<<"select_index">>, Reward_match#reward_match.select_index},
		{<<"max_index">>, Reward_match#reward_match.max_index},
		{<<"max_index_buff">>, Reward_match#reward_match.max_index_buff},
		{<<"combo">>, Reward_match#reward_match.combo},
		{<<"dis_slot">>, Reward_match#reward_match.dis_slot},
		{<<"level">>, Reward_match#reward_match.level},
		{<<"levelmax">>, Reward_match#reward_match.levelmax},
		{<<"hp">>, Reward_match#reward_match.hp},
		{<<"hpmax">>, Reward_match#reward_match.hpmax},
		{<<"card_list">>, CardList},
		{<<"card_list_hid">>, CardHidList},
		{<<"buff_list">>, BuffList},
		{<<"combomax">>, ComboList},
		{<<"levelcount">>, LevelCountList},
		{<<"reward_team">>,RewardTeam}
	    ]
  	}.
	
%%格式化活跃度任务
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
%%格式化任务列表
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

%%格式化玩家任务
encode_json_mission(MissionList, PlayerActivity) when is_list(MissionList) andalso is_record(PlayerActivity, player_activity) ->
  {
    struct,
    [
      {<<"mission_list">>, encode_json_mission_list(MissionList)},
      {<<"activity">>, encode_json_activity(PlayerActivity)}
    ]
  }.

%%格式化单个成就数据
%%Status ：1=>未完成，2=>已完成， 3=>已领取奖励
encode_json_achievement_item(AchievementItem) when is_record(AchievementItem, achievement_item) ->
  Status =
    if
      AchievementItem#achievement_item.finish_ts > 0 andalso AchievementItem#achievement_item.get_reward_ts > 0 -> 3;
      AchievementItem#achievement_item.finish_ts > 0 -> 2;
      true -> 1
    end,
  Stat =
    if
      AchievementItem#achievement_item.statistics > AchievementItem#achievement_item.progress -> AchievementItem#achievement_item.progress;
      true -> AchievementItem#achievement_item.statistics
    end,
  {
    struct,
    [
      {<<"id">>, dd_util:to_binary(AchievementItem#achievement_item.id)},
      {<<"progress">>, AchievementItem#achievement_item.progress},
      {<<"condition">>, AchievementItem#achievement_item.progress},
      {<<"stat">>, Stat},
      {<<"status">>, Status}
    ]
  }.

%%格式化成就列表
encode_json_achievement_list(GroupList) when is_list(GroupList) ->
  lists:map(
    fun(AchievementGroupItem) ->
      {
        struct,
        [
          {<<"group_id">>, AchievementGroupItem#achievement_group.group_id},
          {<<"1">>, encode_json_achievement_item(AchievementGroupItem#achievement_group.item_level_1)},
          {<<"2">>, encode_json_achievement_item(AchievementGroupItem#achievement_group.item_level_2)},
          {<<"3">>, encode_json_achievement_item(AchievementGroupItem#achievement_group.item_level_3)}
        ]
      }
    end, GroupList).

%%格式化新手引导的步骤
encode_json_guide_step(Step) when is_list(Step) ->
  lists:map(
    fun(Item) ->
      {
        struct,
        [
          {<<"key">>, dd_util:to_binary(Item#record_item.key)},
          {<<"val">>, Item#record_item.value}
        ]
      }
    end, Step).

%%格式化商店数据
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

%%格式化抽奖数据
encode_json_lottery(LotteryList) ->
  lists:map(
    fun(Item) ->
      {
        struct,
        [
          {<<"id">>, Item#lottery_conf.id},
          {<<"price">>, Item#lottery_conf.price},
          {<<"is_discount">>, Item#lottery_conf.is_discount},
          {<<"tool_id">>,dd_util:to_binary(Item#lottery_conf.tool_id)}
        ]
      }
    end, LotteryList).


%%格式化抽奖列表
encode_json_lottery_list(LotteryList) when is_list(LotteryList) ->
  encode_json_treasure_list(LotteryList).

%%格式化无尽掉落数据
encode_json_endless_drop(DropList) when is_list(DropList) ->
  encode_json_treasure_list(DropList).

%%格式化关卡掉落数据
encode_json_tollgate_drop(DropList) when is_list(DropList) ->
  encode_json_treasure_list(DropList).

encode_json_tollgate_sweep_drop(DropList) when is_list(DropList) ->
  lists:map(
    fun(DropItemList) ->
      encode_json_treasure_list(DropItemList)
    end, DropList).

encode_json_treasure_list(TreasureList) when is_list(TreasureList) ->
  lists:map(
    fun(Item) ->
      {
        struct,
        [
          {<<"type">>, Item#treasure_item.drop_type},
          {<<"drop_id">>, dd_util:to_binary(Item#treasure_item.drop_id)},
          {<<"count">>, Item#treasure_item.count}
        ]
      }
    end, TreasureList).

%%格式化激活码获取物品
encode_json_cdkey_package(PackageList) when is_list(PackageList) ->
  lists:map(
    fun({Type, ID, Count}) ->
      {
        struct,
        [
          {<<"type">>, Type},
          {<<"id">>, dd_util:to_binary(ID)},
          {<<"count">>, Count}
        ]
      }
    end, PackageList).

%%格式化好友或者其他玩家数据信息
encode_json_basic_info(InfoList) when is_list(InfoList) ->
  lists:map(
    fun(RD) ->
      {Uin, DisName, Atk, Tollgate, EndlessMaxScore, EndlessRank, Tower} = RD,
      {
        struct,
        [
          {<<"id">>, Uin},
          {<<"info">>,
            {
              struct,
              [
                {<<"dis_name">>, dd_util:to_binary(DisName)},
                {<<"atk">>, Atk},
                {<<"max_tollgate">>, Tollgate},
                {<<"endless_max_score">>, EndlessMaxScore},
                {<<"endless_rank">>, EndlessRank},
                {<<"tower">>, encode_json_select_tower(Tower)}
              ]
            }
          }
        ]
      }
    end, InfoList).

%%格式化好友无尽排名数据
encode_json_friend_endless_rank(List) when is_list(List) ->
  lists:map(
    fun({RankInfo, Phy}) ->
      {
        struct,
        [
          {<<"uin">>, RankInfo#rank_info.uin},
          {<<"id">>, dd_util:to_binary(RankInfo#rank_info.uname)},
%%          {<<"uname">>, dd_util:to_binary(RankInfo#rank_info.dis_name)},
          {<<"uname">>, dd_util:to_binary("PlayerID:" ++ dd_util:to_list(RankInfo#rank_info.uin))},
          {<<"score">>, RankInfo#rank_info.score},
          {<<"rank">>, RankInfo#rank_info.rank},
          {<<"phy_flag">>, Phy}
        ]
      }
    end, List).

%%格式化全服无尽排名数据
encode_json_server_endless_rank(Top20List, {Rank, Point, TotalNum}) when is_list(Top20List) ->
  RankList =
    lists:map(
      fun(RankInfo) ->
        {
          struct,
          [
            {<<"uin">>, RankInfo#rank_info.uin},
            {<<"uname">>, dd_util:to_binary(RankInfo#rank_info.uname)},
            {<<"dis_name">>, dd_util:to_binary(RankInfo#rank_info.dis_name)},
            {<<"score">>, RankInfo#rank_info.score},
            {<<"rank">>, RankInfo#rank_info.rank}
          ]
        }
      end, Top20List),
  SelfJson =
    {
      struct,
      [
        {<<"rank">>, Rank},
        {<<"score">>, Point},
        {<<"total_number">>, TotalNum}
      ]
    },
  {
    struct,
    [
      {<<"top20">>, RankList},
      {<<"self">>, SelfJson}
    ]
  }.

%%格式化无尽数据和排名
encode_json_endless_tollgate(EndLess, _FriendRankList) when is_record(EndLess, endless_tollgate) ->
  EndlessTollgate =
    {
      struct,
      [
        {<<"max_score">>, EndLess#endless_tollgate.max_score},
        {<<"last_week_max_score">>, EndLess#endless_tollgate.last_week_max_score},
        {<<"this_week_max_score">>, EndLess#endless_tollgate.this_week_max_score}
      ]
    },
  {
    struct,
    [
      {<<"endless_tollgate_info">>, EndlessTollgate},
      {<<"tollgate_rank">>, []}
    ]
  }.

%%格式化普通关卡数据和排名（前三名）
encode_json_tollgate_mode(UpdateTollgate, FriendRankList) when is_record(UpdateTollgate, tollgate) ->
  UpdateTollgateJson =
    {
      struct,
      [
        {<<"id">>, UpdateTollgate#tollgate.id},
        {<<"max_score">>, UpdateTollgate#tollgate.max_score},
        {<<"star_seq">>, UpdateTollgate#tollgate.max_star_seq},
        {<<"max_star">>, UpdateTollgate#tollgate.max_star},
        {<<"remain_times">>, UpdateTollgate#tollgate.daily_remain_times},
        {<<"addition_times">>, UpdateTollgate#tollgate.addition_remain_times}
      ]
    },
  RankList =
    lists:map(
      fun(RankInfo) ->
        {
          struct,
          [
            {<<"uin">>, RankInfo#rank_info.uin},
            {<<"uname">>, dd_util:to_binary(RankInfo#rank_info.uname)},
            {<<"dis_name">>, dd_util:to_binary("PlayerID:" ++ dd_util:to_list(RankInfo#rank_info.uin))},
            %%{<<"dis_name">>, dd_util:to_binary(RankInfo#rank_info.dis_name)},
            {<<"score">>, RankInfo#rank_info.score},
            {<<"rank">>, RankInfo#rank_info.rank}
          ]
        }
      end, FriendRankList),
  {
    struct,
    [
      {<<"update_tollgate_info">>, UpdateTollgateJson},
      {<<"tollgate_rank">>, RankList}
    ]
  }.

%%格式化活动关卡数据和排名
encode_json_activity_tollgate(ActivityTollgate, FriendRankList) when is_record(ActivityTollgate, activity_tollgate_item) ->
  UpdateTollgateJson =
    {
      struct,
      [
        {<<"id">>, ActivityTollgate#activity_tollgate_item.tollgate_id},
        {<<"max_score">>, ActivityTollgate#activity_tollgate_item.max_score},
        {<<"max_star">>, ActivityTollgate#activity_tollgate_item.max_star},
        {<<"remain_times">>, ActivityTollgate#activity_tollgate_item.daily_remain_times}
      ]
    },
  RankList =
    lists:map(
      fun(RankInfo) ->
        {
          struct,
          [
            {<<"uin">>, RankInfo#rank_info.uin},
            {<<"uname">>, dd_util:to_binary(RankInfo#rank_info.uname)},
            {<<"dis_name">>, dd_util:to_binary(RankInfo#rank_info.dis_name)},
            {<<"score">>, RankInfo#rank_info.score},
            {<<"rank">>, RankInfo#rank_info.rank}
          ]
        }
      end, FriendRankList),
  {
    struct,
    [
      {<<"activity_tollgate_info">>, UpdateTollgateJson},
      {<<"tollgate_rank">>, RankList}
    ]
  }.