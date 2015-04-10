%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 七月 2014 下午5:20
%%%-------------------------------------------------------------------
-module(database_util).
-author("zqlt").

-include("../../deps/file_log/include/file_log.hrl").
-include("../../deps/mysql/include/mysql.hrl").
-include("../cache/cache_def.hrl").
-include("../csv.hrl").
-include("../mail/mail.hrl").
%% API
-export([
  init_account/1,
  decode_account_rd/1,
  get_create_account_sql/1,
  update_account_sql/2]).

-export([
  decode_stage/1,
  decode_platform_info/1,
  decode_addition/1,
  decode_strength/1,
  decode_shop/1,
  decode_hero/1,
  decode_login_reward/1,
  decode_notice_rd/1,
  create_notice_sql/1,
  update_notice_sql/1
]).

-export([
  decode_mail_template_rd/1,
  decode_bulletin_mail_rd/1,
  decode_attach_mail_rd/1
]).

-export([
  get_create_mail_template_sql/1,
  get_create_attach_mail_sql/1,
  get_create_bulletin_mail_sql/1
]).

-export([
  get_create_good_sql/1,
  decode_shop_config_item/1,
  decode_lottery_config_item/1
]).

init_account(Uin) when is_integer(Uin) ->
  CurTime = dd_util:timestamp(),
  #account{
    uin = Uin,
	players = #players{players_lists = [],select_players = []},
    gold_coin = 0,
    gem = 0,
    platform_info = #platform_info{player_id = "", player_dis_name = "", plat_type = "", player_friends = []},
    heros = #heros{character_lists = [], select_hero = []},
    backpack = #backpack{equipment_list = [], equip_piece = gb_trees:empty(), material_list = gb_trees:empty(), prop_list = gb_trees:empty(), inscription_list = [], inscription_piece_list = gb_trees:empty(), capacity = 0
						,s_equipment_list = gb_trees:empty()
						,s_material_list = gb_trees:empty()
						,s_fragment_list = gb_trees:empty()
						,s_consumables_list = gb_trees:empty()
						,s_card_list = gb_trees:empty()},
    mission = #mission{ mission_list = [], player_activity = #player_activity{activity_value = 0, activity_reward_list = []}
    },
    achievement = #achievement{group_list = [], last_update_ts = 0},
    shop = #shop{goods_list = [], permanent_buy_list = [], last_update_ts = dd_util:timestamp(), pay_info = #pay_info{pay_record = [], first_pay_ts = 0, total_pay_val = 0, total_gem_count = 0}},
    stage = cache_account_def:init_stage(),
    login_reward = #login_reward{login_times = 1, latest_login_ts = dd_util:timestamp(), login_reward_list = [], total_login_days = 0},
    strength = #strength{strength = 0, last_update_ts = CurTime, give_friend_strength = [], close_strength_gift = 1, today_buy_times = 0},
    lottery = #lottery{last_lottery_ts = 0, today_lottery_lists = [], single_lottery_times = 0, ten_lottery_times = 0},
    guild = #guild{},
    competitive = #competitive{},
    addition = #addition{newer_guide_steps = 1000, rank_reward_got = 0, guide_steps = [], cdkey_pack_list = gb_trees:empty()},
    create_ts = CurTime,
    last_login_ts = CurTime,
	reward_match = #reward_match{},
	reward_match_ts = #ts_item{count=5,last_update_ts = CurTime}
  }.



get_create_account_sql(Account) when is_record(Account, account) ->
   mysql_util:insert_query(
    "account",
     ["uin","player","gold_coin", "gem", "platform_info", "hero", "backpack", "mission", "achievement", "shop", "stage", "login_reward", "strength", "lottery", "guild", "competitive", "addition", "create_ts", "last_login_ts","reward_match","reward_match_ts"],
     [
       encode_uin(Account#account.uin),
	   encode_player(Account#account.players),
       encode_gold_coin(Account#account.gold_coin),
       encode_gem(Account#account.gem),
       encode_platform_info(Account#account.platform_info),
       encode_hero(Account#account.heros),
       encode_backpack(Account#account.backpack),
       encode_mission(Account#account.mission),
       encode_achievement(Account#account.achievement),
       encode_shop(Account#account.shop),
       encode_stage(Account#account.stage),
       encode_login_reward(Account#account.login_reward),
       encode_strength(Account#account.strength),
       encode_lottery(Account#account.lottery),
       encode_guild(Account#account.guild),
       encode_competitive(Account#account.competitive),
       encode_addition(Account#account.addition),
       encode_create_ts(Account#account.create_ts),
       encode_last_login_ts(Account#account.last_login_ts),
	   encode_reward_match(Account#account.reward_match),
	   encode_ts_item(Account#account.reward_match_ts)
     ]
   ).

update_account_sql(Account, FieldList) ->
  ValueList =
    lists:map(
      fun(Field) ->
        if
		  Field =:= "player" -> encode_player(Account#account.players);
          Field =:= "platform_info" -> encode_platform_info(Account#account.platform_info);
          Field =:= "gold_coin" -> encode_gold_coin(Account#account.gold_coin);
          Field =:= "gem" -> encode_gem(Account#account.gem);
          Field =:= "hero" -> encode_hero(Account#account.heros);
          Field =:= "backpack" -> encode_backpack(Account#account.backpack);
          Field =:= "mission" -> encode_mission(Account#account.mission);
          Field =:= "achievement" -> encode_achievement(Account#account.achievement);
          Field =:= "shop" -> encode_shop(Account#account.shop);
          Field =:= "stage" -> encode_stage(Account#account.stage);
          Field =:= "login_reward" -> encode_login_reward(Account#account.login_reward);
          Field =:= "strength" -> encode_strength(Account#account.strength);
          Field =:= "lottery" -> encode_lottery(Account#account.lottery);
          Field =:= "guild" -> encode_guild(Account#account.guild);
          Field =:= "competitive" -> encode_competitive(Account#account.competitive);
          Field =:= "addition" -> encode_addition(Account#account.addition);
          Field =:= "create_ts" -> encode_create_ts(Account#account.create_ts);
          Field =:= "last_login_ts" -> encode_last_login_ts(Account#account.last_login_ts);
		  Field =:= "reward_match" -> encode_reward_match(Account#account.reward_match);
          Field =:= "reward_match_ts" -> encode_ts_item(Account#account.reward_match_ts);
          true -> throw({custom, "error field name" ++ Field})
        end
    end, FieldList),
  mysql_util:update_query("account", FieldList, ValueList, "uin=" ++ dd_util:to_list(Account#account.uin)).

encode_player(Players) when is_record(Players, players) ->
  SelectPlayers =
    lists:map(
      fun({Pos, ID}) ->
        {
          struct,
          [
            {<<"pos">>, Pos},
            {<<"id">>,ID}
          ]
        }
      end, Players#players.select_players),

  Json =
    {
      struct,
      [
        {<<"sel">>, SelectPlayers}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_uin(Uin) when is_integer(Uin)-> dd_util:to_list(Uin).
encode_gold_coin(GoldCoin) when is_integer(GoldCoin) -> dd_util:to_list(GoldCoin).
encode_gem(Gem) when is_integer(Gem) -> dd_util:to_list(Gem).
encode_platform_info(PlatformInfo) when is_record(PlatformInfo, platform_info) ->
  FriendJsonList =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"id">>, dd_util:to_binary(Item#friend_item.id)},
            {<<"name">>, dd_util:to_binary(Item#friend_item.dis_name)},
            {<<"uin">>, Item#friend_item.uin}
          ]
        }
      end, PlatformInfo#platform_info.player_friends),
  Json =
    {
      struct,
      [
        {<<"plat">>, dd_util:to_binary(PlatformInfo#platform_info.plat_type)},
        {<<"id">>, dd_util:to_binary(PlatformInfo#platform_info.player_id)},
        {<<"name">>, dd_util:to_binary(PlatformInfo#platform_info.player_dis_name)},
        {<<"friend">>, FriendJsonList}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_hero(Heros) when is_record(Heros, heros) ->
  L = lists:map(
    fun(HeroItem) ->
      EquipL =
        lists:map(
          fun(EquipItem) ->
            {
              struct,
              [
                {<<"id">>, dd_util:to_binary(EquipItem#equipment.id)},
                {<<"no">>, dd_util:to_binary(EquipItem#equipment.no)},
                {<<"exp">>,EquipItem#equipment.exp}
              ]
            }
          end, HeroItem#character.equipment_list),
      Inscription =
        lists:map(
          fun(Ins) ->
            {
              struct,
              [
                {<<"pos">>, Ins#inscription.pos},
                {<<"id">>, dd_util:to_binary(Ins#inscription.id)}
              ]
            }
          end, HeroItem#character.inscription_list),

      {
        struct,
        [
          {<<"id">>, dd_util:to_binary(HeroItem#character.id)},
          {<<"equipment">>, EquipL},
          {<<"inscription">>, Inscription}
        ]
      }
    end, Heros#heros.character_lists),

  SelectHero =
    lists:map(
      fun({Pos, ID}) ->
        {
          struct,
          [
            {<<"pos">>, Pos},
            {<<"id">>, dd_util:to_binary(ID)}
          ]
        }
      end, Heros#heros.select_hero),

  Json =
    {
      struct,
      [
        {<<"hero">>, L},
        {<<"sel">>, SelectHero}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_backpack(BackPack) when is_record(BackPack, backpack) ->
  EquipL = [],
%%     lists:map(
%%       fun(EquipItem) ->
%%         {
%%           struct,
%%           [
%%             {<<"id">>, dd_util:to_binary(EquipItem#equipment.id)},
%%             {<<"no">>, dd_util:to_binary(EquipItem#equipment.no)},
%%             {<<"exp">>,EquipItem#equipment.exp}
%%           ]
%%         }
%%       end, []),
%%      end, BackPack#backpack.equipment_list), 2014.12.20 丢弃该字段
  MaterialL =  [],
%%     lists:map(
%%      fun(MaterialItem) ->
%%        {
%%          struct,
%%          [
%%            {<<"id">>, dd_util:to_binary(MaterialItem#material.id)},
%%            {<<"count">>, MaterialItem#material.count}
%%          ]
%%        }
%%      end, []),
%%     end, BackPack#backpack.material_list),
  PropList =
    lists:map(
      fun({ID, Count}) ->
        {
          struct,
          [
            {<<"id">>, dd_util:to_binary(ID)},
            {<<"count">>, Count}
          ]
        }
      end, gb_trees:to_list(BackPack#backpack.prop_list)),

  EquipPiece =
    lists:map(
      fun({ID, Count}) ->
        {
          struct,
          [
            {<<"id">>, dd_util:to_binary(ID)},
            {<<"count">>, Count}
          ]
        }
      end, gb_trees:to_list(BackPack#backpack.equip_piece)),

  InscriptionPiece =
    lists:map(
      fun({ID, Count}) ->
        {
          struct,
          [
            {<<"id">>, dd_util:to_binary(ID)},
            {<<"count">>, Count}
          ]
        }
      end, gb_trees:to_list(BackPack#backpack.inscription_piece_list)),

  Inscription =
    lists:map(
      fun(Ins) ->
        {
          struct,
          [
            {<<"pos">>, Ins#inscription.pos},
            {<<"id">>, dd_util:to_binary(Ins#inscription.id)}
          ]
        }
      end, BackPack#backpack.inscription_list),
	%superstart
  S_Equip =
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
 S_Material =
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
  S_Fragment =
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
  S_Consumables =
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
  S_Card =
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
  
  Json =
    {
      struct,
      [
        {<<"equipment">>, EquipL},
        {<<"material">>, MaterialL},
        {<<"property">>, PropList},
        {<<"e_p">>, EquipPiece},
        {<<"i_p">>, InscriptionPiece},
        {<<"inscription">>, Inscription},
        {<<"capacity">>, BackPack#backpack.capacity},
		%superstar属性
		{<<"s_equipment">>, S_Equip},
		{<<"s_material">>, S_Material},
		{<<"s_fragment">>, S_Fragment},
		{<<"s_consumables">>, S_Consumables},
		{<<"s_card">>, S_Card}
      ]
   },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_reward_match(Reward_match) when is_record(Reward_match, reward_match) ->
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
	lists:map(
		fun({Pos,PlayerId})->
				{
					struct,
			          [
			            {<<"pos">>, Pos},
			            {<<"id">>, PlayerId}
			          ]	 
				}
		end,Reward_match#reward_match.reward_team),
  Json =
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
  },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_ts_item(Ts_item) when is_record(Ts_item, ts_item) ->
 Json =
    {
      struct,
      [
        {<<"count">>, Ts_item#ts_item.count},
        {<<"last_update_ts">>, Ts_item#ts_item.last_update_ts}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_mission_item(MissionItem) when is_record(MissionItem, mission_item) ->
  RecordList =
    lists:map(
      fun(RecordItem) ->
        {
          struct,
          [
            {<<"key">>, dd_util:to_binary(RecordItem#record_item.key)},
            {<<"val">>, RecordItem#record_item.value}
          ]
        }
      end, MissionItem#mission_item.mission_record),
  {
    struct,
    [
      {<<"id">>, dd_util:to_binary(MissionItem#mission_item.mission_id)},
      {<<"p1">>, MissionItem#mission_item.mission_progress_1},
      {<<"p2">>, MissionItem#mission_item.mission_progress_2},
      {<<"fts">>, MissionItem#mission_item.mission_finish_ts},
      {<<"rts">>, MissionItem#mission_item.mission_get_reward_ts},
      {<<"rd">>, RecordList},
      {<<"type">>, MissionItem#mission_item.mission_type},
      {<<"fcnt">>, MissionItem#mission_item.mission_finish_times}
    ]
  }.

encode_mission(Mission) when is_record(Mission, mission) ->
  MissionList =
    lists:map(
      fun(MissionItem) ->
        encode_mission_item(MissionItem)
      end, Mission#mission.mission_list),
  ActivityRewardList =
    lists:map(
      fun(ActivityRewardItem) ->
        {
          struct,
          [
            {<<"id">>, ActivityRewardItem#activity_reward_item.id},
            {<<"fts">>, ActivityRewardItem#activity_reward_item.finish_ts},
            {<<"rts">>, ActivityRewardItem#activity_reward_item.get_reward_ts}
          ]
        }
      end, Mission#mission.player_activity#player_activity.activity_reward_list),
  PlayerActivity =
    {
      struct,
      [
        {<<"val">>, Mission#mission.player_activity#player_activity.activity_value},
        {<<"al">>, ActivityRewardList}
      ]
    },
  Json =
    {
      struct,
      [
        {<<"ml">>, MissionList},
        {<<"ac">>, PlayerActivity}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_achievement_item(AchievementItem) when is_record(AchievementItem, achievement_item) ->
  RecordList =
    lists:map(
      fun(RecordItem) ->
        {
          struct,
          [
            {<<"key">>, dd_util:to_binary(RecordItem#record_item.key)},
            {<<"val">>, RecordItem#record_item.value}
          ]
        }
      end, AchievementItem#achievement_item.record),
  {
    struct,
    [
      {<<"id">>, dd_util:to_binary(AchievementItem#achievement_item.id)},
      {<<"s1">>, AchievementItem#achievement_item.statistics},
      {<<"s2">>, AchievementItem#achievement_item.statistics_1},
      {<<"rd">>, RecordList},
      {<<"fts">>, AchievementItem#achievement_item.finish_ts},
      {<<"rts">>, AchievementItem#achievement_item.get_reward_ts},
      {<<"pv">>, AchievementItem#achievement_item.progress}
    ]
  }.

encode_achievement(Achievement) when is_record(Achievement, achievement) ->
  GroupList =
    lists:map(
      fun(GroupItem) ->
        {
          struct,
          [
            {<<"id">>, GroupItem#achievement_group.group_id},
            {<<"1">>, encode_achievement_item(GroupItem#achievement_group.item_level_1)},
            {<<"2">>, encode_achievement_item(GroupItem#achievement_group.item_level_2)},
            {<<"3">>, encode_achievement_item(GroupItem#achievement_group.item_level_3)}
          ]
        }
      end, Achievement#achievement.group_list),
  Json =
    {
      struct,
      [
        {<<"gl">>, GroupList},
        {<<"uts">>, Achievement#achievement.last_update_ts}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).


encode_shop(Shop) when is_record(Shop, shop) ->
  List =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"id">>, dd_util:to_binary(Item#goods.id)},
            {<<"rcnt">>, Item#goods.remain_count},
            {<<"bts">>, Item#goods.latest_buy_ts}
          ]
        }
      end, Shop#shop.goods_list),
  PermanentList =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"id">>, dd_util:to_binary(Item#goods.id)},
            {<<"rcnt">>, Item#goods.remain_count},
            {<<"bts">>, Item#goods.latest_buy_ts}
          ]
        }
      end, Shop#shop.permanent_buy_list),
  PayRecord =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"ts">>, Item#pay_item.pay_ts},
            {<<"id">>, dd_util:to_binary(Item#pay_item.pay_item_id)},
            {<<"gem">>, Item#pay_item.pay_gem_count},
            {<<"pp">>, dd_util:to_binary(dd_util:float_to_string(Item#pay_item.pay_price))}
          ]
        }
      end, Shop#shop.pay_info#pay_info.pay_record),
  PayInfo =
    {
      struct,
      [
        {<<"fp">>, Shop#shop.pay_info#pay_info.first_pay_ts},
        {<<"tp">>, dd_util:to_binary(dd_util:float_to_string(Shop#shop.pay_info#pay_info.total_pay_val))},
        {<<"tg">>, Shop#shop.pay_info#pay_info.total_gem_count},
        {<<"pr">>, PayRecord}
      ]
    },
  Json =
    {
      struct,
      [
        {<<"gl">>, List},
        {<<"pgl">>, PermanentList},
        {<<"uts">>, Shop#shop.last_update_ts},
        {<<"pi">>, PayInfo}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_stage(Stage) when is_record(Stage, stage) ->
  BaseTollgateJsonList =
    lists:map(
      fun(Tollgate) ->
        {
          struct,
          [
            {<<"id">>, Tollgate#tollgate.id},
            {<<"msc">>, Tollgate#tollgate.max_score},
            {<<"mst">>, Tollgate#tollgate.max_star},
            {<<"msts">>, Tollgate#tollgate.max_star_seq},
            {<<"rcnt">>, Tollgate#tollgate.daily_remain_times},
            {<<"arcnt">>, Tollgate#tollgate.addition_remain_times},
            {<<"pts">>, Tollgate#tollgate.last_pass_ts}
          ]
        }
      end, Stage#stage.base_tollgate_list),
  TollgateDrop =
    {
      struct,
      [
        {<<"sval">>, Stage#stage.tollgate_drop#tollgate_drop.seed_val},
        {<<"rval">>, Stage#stage.tollgate_drop#tollgate_drop.rand_val},
        {<<"sts">>, Stage#stage.tollgate_drop#tollgate_drop.seed_ts},
        {<<"ets">>, Stage#stage.tollgate_drop#tollgate_drop.end_ts}
      ]
    },
  HarvestObstacle =
    lists:map(
      fun(HarvestItem) ->
        {
          struct,
          [
            {<<"id">>, dd_util:to_binary(HarvestItem#harvest_obstacles_item.id)},
            {<<"rcnt">>, HarvestItem#harvest_obstacles_item.remain_number},
            {<<"uts">>, HarvestItem#harvest_obstacles_item.last_update_ts}
          ]
        }
      end, Stage#stage.harvest_obstacles_list),
  ActivityList =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"id">>, Item#activity_tollgate_item.tollgate_id},
            {<<"msc">>, Item#activity_tollgate_item.max_score},
            {<<"mst">>, Item#activity_tollgate_item.max_star},
            {<<"msts">>, Item#activity_tollgate_item.max_star_seq},
            {<<"rcnt">>, Item#activity_tollgate_item.daily_remain_times},
            {<<"fts">>, Item#activity_tollgate_item.last_finish_ts}
          ]
        }
      end, Stage#stage.ac_tollgate),
  WeekRecordList =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"time">>, Item#endless_item.time},
            {<<"score">>, Item#endless_item.score},
            {<<"tid">>, Item#endless_item.tollgate_id}
          ]
        }
      end, Stage#stage.endless_tollgate#endless_tollgate.week_score_record),
  EndlessDropList =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"id">>, Item#treasure_item.treasure_id},
            {<<"sid">>, Item#treasure_item.sub_id},
            {<<"did">>, dd_util:to_binary(Item#treasure_item.drop_id)},
            {<<"type">>, Item#treasure_item.drop_type},
            {<<"cnt">>, Item#treasure_item.count},
            {<<"lvl">>, Item#treasure_item.drop_level}
          ]
        }
      end, Stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.endless_drop),
  EndlessDrop =
    {
      struct,
      [
        {<<"sval">>, Stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.seed_val},
        {<<"rval">>, Stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.rand_val},
        {<<"sts">>, Stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.seed_ts},
        {<<"ets">>, Stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.end_ts},
        {<<"dl">>, EndlessDropList}
      ]
    },
  EndlessTollgate =
    {
      struct,
      [
        {<<"msc">>, Stage#stage.endless_tollgate#endless_tollgate.max_score},
        {<<"mgts">>, Stage#stage.endless_tollgate#endless_tollgate.max_score_gain_ts},
        {<<"mwc">>, Stage#stage.endless_tollgate#endless_tollgate.max_wave_count},
        {<<"lwsc">>, Stage#stage.endless_tollgate#endless_tollgate.last_week_max_score},
        {<<"lgts">>, Stage#stage.endless_tollgate#endless_tollgate.last_week_max_score_gain_ts},
        {<<"twsc">>, Stage#stage.endless_tollgate#endless_tollgate.this_week_max_score},
        {<<"tgts">>, Stage#stage.endless_tollgate#endless_tollgate.this_week_max_score_gain_ts},
        {<<"uts">>, Stage#stage.endless_tollgate#endless_tollgate.last_update_ts},
        {<<"wrdl">>, WeekRecordList},
        {<<"ed">>, EndlessDrop}
      ]
    },
  Json =
    {
      struct,
      [
        {<<"bt">>, BaseTollgateJsonList},
        {<<"td">>, TollgateDrop},
        {<<"ho">>, HarvestObstacle},
        {<<"at">>, ActivityList},
        {<<"et">>, EndlessTollgate}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_login_reward(LoginReward) when is_record(LoginReward, login_reward) ->
  RewardList =
    lists:map(
     fun(RewardItem) ->
       {
         struct,
         [
           {<<"id">>, RewardItem#login_reward_item.id},
           {<<"lts">>, RewardItem#login_reward_item.login_ts}
         ]
       }
     end, LoginReward#login_reward.login_reward_list),

  Json =
    {
      struct,
      [
        {<<"llts">>, LoginReward#login_reward.latest_login_ts},
        {<<"lcnt">>, LoginReward#login_reward.login_times},
        {<<"rl">>, RewardList},
        {<<"td">>, dd_util:to_integer(LoginReward#login_reward.total_login_days)}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_strength(Strength) when is_record(Strength, strength) ->
 GiveStrength =
  lists:map(
    fun(Item) ->
      {
        struct,
        [
          {<<"val">>, Item#strength_item.give_strength_value},
          {<<"src">>, Item#strength_item.give_strength_source},
          {<<"dst">>, Item#strength_item.give_strength_dest},
          {<<"ts">>, Item#strength_item.give_strength_ts}
        ]
      }
    end, Strength#strength.give_friend_strength),
 Json =
    {
      struct,
      [
        {<<"sval">>, Strength#strength.strength},
        {<<"gsth">>, GiveStrength},
        {<<"uts">>, Strength#strength.last_update_ts},
        {<<"gcls">>, Strength#strength.close_strength_gift},
        {<<"bcnt">>, Strength#strength.today_buy_times}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_lottery(Lottery) when is_record(Lottery, lottery) ->
  List =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"id">>, Item#treasure_item.treasure_id},
            {<<"sid">>, Item#treasure_item.sub_id},
            {<<"did">>, dd_util:to_binary(Item#treasure_item.drop_id)},
            {<<"type">>, Item#treasure_item.drop_type},
            {<<"cnt">>, Item#treasure_item.count},
            {<<"lvl">>, Item#treasure_item.drop_level}
          ]
        }
      end, Lottery#lottery.today_lottery_lists),
  Json =
    {
      struct,
      [
        {<<"ll">>, List},
        {<<"ts">>, Lottery#lottery.last_lottery_ts},
        {<<"slt">>,  Lottery#lottery.single_lottery_times},
        {<<"tlt">>, Lottery#lottery.ten_lottery_times}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_guild(Guild) when is_record(Guild, guild)->
  Json =
    {
      struct,
      [
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_competitive(Competitive) when is_record(Competitive, competitive) ->
  Json =
    {
      struct,
      [
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_addition(Addition) when is_record(Addition, addition) ->
  GuideList =
    lists:map(
      fun(Item) ->
        {
          struct,
          [
            {<<"k">>,  dd_util:to_binary(Item#record_item.key)},
            {<<"v">>, Item#record_item.value}
          ]
        }
      end, Addition#addition.guide_steps),
  PackList =
    lists:map(
      fun({ID, Cnt}) ->
        {
          struct,
          [
            {<<"k">>, dd_util:to_binary(ID)},
            {<<"v">>, Cnt}
          ]
        }
      end, gb_trees:to_list(Addition#addition.cdkey_pack_list)),
  Json =
    {
      struct,
      [
        {<<"gs">>, Addition#addition.newer_guide_steps},
        {<<"rrg">>, Addition#addition.rank_reward_got},
        {<<"ngs">>, GuideList},
        {<<"pl">>, PackList}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_create_ts(CreateTs) when is_integer(CreateTs) ->
  dd_util:to_list(CreateTs).

encode_last_login_ts(LastLoginTs) when is_integer(LastLoginTs) ->
  dd_util:to_list(LastLoginTs).



decode_account_rd([Uin, Players,GoldCoin, Gem, PlatformInfo, Hero, Backpack, Mission, Achievement, Shop, Stage, LoginReward, Strength, Lottery, Guild, Competitive, Addition, CreateTs, LastLoginTs,Reward_match,Reward_match_ts]) ->
  #account{
    uin = decode_uin(Uin),
	players = decode_player(Players),
    gold_coin = decode_gold_coin(GoldCoin),
    gem = decode_gem(Gem),
    platform_info = decode_platform_info(PlatformInfo),
    heros = decode_hero(Hero),
    backpack = decode_back_pack(Backpack),
    mission = decode_mission(Mission),
    achievement = decode_achievement(Achievement),
    shop = decode_shop(Shop),
    stage = decode_stage(Stage),
    login_reward = decode_login_reward(LoginReward),
    strength = decode_strength(Strength),
    lottery = decode_lottery(Lottery),
    guild = decode_guild(Guild),
    competitive = decode_competitive(Competitive),
    addition = decode_addition(Addition),
    create_ts = decode_create_ts(CreateTs),
    last_login_ts = decode_last_login_ts(LastLoginTs),
	reward_match = decode_reward_match(Reward_match),
	reward_match_ts = decode_ts_item(Reward_match_ts)
  }.

decode_uin(Uin) -> dd_util:to_integer(Uin).
decode_player(Players) ->
  {struct, PlayerDataJsonList} = mochijson2:decode(Players),
  SelectPlayerJson = get_json_value_without_exception(<<"sel">>, PlayerDataJsonList, []),
  SelectPlayer =
    case SelectPlayerJson of
      [] -> [{0,0},{0,0},{0,0}];
      undefined -> [{0,0},{0,0},{0,0}];
      _ ->
        lists:map(
          fun({struct, Item}) ->
            SPos = dd_util:to_integer(get_json_value(<<"pos">>, Item)),
            SID = dd_util:to_integer(get_json_value(<<"id">>, Item)),
            {SPos, SID}
          end, SelectPlayerJson)
    end,
  #players{players_lists = [], select_players = SelectPlayer}.

decode_gold_coin(GoldCoin) ->dd_util:to_integer(GoldCoin).
decode_gem(Gem) -> dd_util:to_integer(Gem).
decode_platform_info(PlatformInfo) ->
  {struct, InfoJsonList} = mochijson2:decode(PlatformInfo),
  PlayerID = dd_util:to_list(get_json_value_without_exception(<<"id">>, InfoJsonList, "")),
  DisName = dd_util:to_list(get_json_value_without_exception(<<"name">>, InfoJsonList, "")),
  Plat = dd_util:to_list(get_json_value_without_exception(<<"plat">>, InfoJsonList, "")),
  FriendJsonList = get_json_value_without_exception(<<"friend">>, InfoJsonList, []),
  FriendList =
    lists:map(
      fun({struct, Item}) ->
        ID = dd_util:to_list(get_json_value(<<"id">>, Item)),
        Name = dd_util:to_list(get_json_value(<<"name">>, Item)),
        FriendUin = dd_util:to_integer(get_json_value(<<"uin">>, Item)),
        #friend_item{id = ID, dis_name = Name, uin = FriendUin}
      end, FriendJsonList),
  #platform_info{player_id = PlayerID, player_dis_name = DisName, plat_type = Plat, player_friends = FriendList}.

decode_hero(Hero) ->
  {struct, HeroDataJsonList} = mochijson2:decode(Hero),
  HeroJsonList = get_json_value(<<"hero">>, HeroDataJsonList),
  SelectHeroJson = get_json_value_without_exception(<<"sel">>, HeroDataJsonList, []),
  HeroList =
    lists:map(
      fun({struct, HeroItemValue}) ->
        Id = get_json_value(<<"id">>, HeroItemValue),
        EquipJsonList = get_json_value(<<"equipment">>, HeroItemValue),
        InscriptionJsonList = get_json_value_without_exception(<<"inscription">>, HeroItemValue, []),
        EquipList = lists:map(
          fun({struct, EquipItem}) ->
            EquipId = get_json_value(<<"id">>, EquipItem),
            EquipNo = get_json_value(<<"no">>, EquipItem),
            {struct, EquipLevel} = get_json_value_without_exception(<<"level">>, EquipItem, {struct, []}),
            EquipOldExp = dd_util:to_integer(get_json_value_without_exception(<<"cur_exp">>, EquipLevel, 0)),
            EquipExp = dd_util:to_integer(get_json_value_without_exception(<<"exp">>, EquipItem, 0)),
            Exp =
              if
                EquipOldExp > EquipExp -> EquipOldExp;
                true -> EquipExp
              end,
            #equipment{id = dd_util:to_list(EquipId), no = dd_util:to_list(EquipNo), exp = Exp}
          end, EquipJsonList),
        InscriptionList =
          lists:map(
            fun({struct, Item}) ->
              Pos = get_json_value(<<"pos">>, Item),
              ID = get_json_value(<<"id">>, Item),
              #inscription{pos = dd_util:to_integer(Pos), id = dd_util:to_list(ID)}
            end, InscriptionJsonList),

        #character{id = dd_util:to_list(Id), equipment_list = EquipList, inscription_list = InscriptionList}
      end, HeroJsonList),
  SelectHero =
    case SelectHeroJson of
      [] -> [{1, "T01"}, {2, "T02"}, {3, "T03"}, {4, "T04"}, {5, "T05"}];
      undefined -> [{1, "T01"}, {2, "T02"}, {3, "T03"}, {4, "T04"}, {5, "T05"}];
      _ ->
        lists:map(
          fun({struct, Item}) ->
            SPos = dd_util:to_integer(get_json_value(<<"pos">>, Item)),
            SID = dd_util:to_list(get_json_value(<<"id">>, Item)),
            {SPos, SID}
          end, SelectHeroJson)
    end,
  #heros{character_lists = HeroList, select_hero = SelectHero}.

ss_d_item_list(List)->
	lists:foldl(
    fun({struct, Item}, TmpTree) ->
      ID = dd_util:to_integer(get_json_value(<<"id">>, Item)),
      COUNT = dd_util:to_integer(get_json_value(<<"count">>, Item)),
	  UUID = dd_util:to_list(get_json_value(<<"uuid">>, Item)),
      case gb_trees:lookup(ID, TmpTree) of
        none ->
          gb_trees:insert(ID, {UUID,COUNT}, TmpTree);
        {value, _} -> TmpTree  %%存在重复的，不作处理
      end
    end, gb_trees:empty(), List).

decode_back_pack(Backpack) ->
  {struct, BackPackList} = mochijson2:decode(Backpack),
%%  EquipJsonList = get_json_value(<<"equipment">>, BackPackList),
%%  MaterialJsonList = get_json_value(<<"material">>, BackPackList),
  PropJsonList = get_json_value(<<"property">>, BackPackList),
  Capacity = get_json_value(<<"capacity">>, BackPackList),
  EquipPieceJsonList = get_json_value_without_exception(<<"e_p">>, BackPackList, []),
  InscriptionPieceJsonList = get_json_value_without_exception(<<"i_p">>, BackPackList, []),
  InscriptionJsonList = get_json_value_without_exception(<<"inscription">>, BackPackList, []),

  S_equipment = get_json_value(<<"s_equipment">>, BackPackList),
  S_material = get_json_value(<<"s_material">>, BackPackList),
  S_fragment = get_json_value(<<"s_fragment">>, BackPackList),
  S_consumables = get_json_value(<<"s_consumables">>, BackPackList),
  S_card = get_json_value(<<"s_card">>, BackPackList),
  
%%   EquipList = lists:map(
%%     fun({struct, EquipItem}) ->
%%       EquipId = get_json_value(<<"id">>, EquipItem),
%%       EquipNo = get_json_value(<<"no">>, EquipItem),
%%       {struct, EquipLevel} = get_json_value_without_exception(<<"level">>, EquipItem, {struct, []}),
%%       EquipOldExp = dd_util:to_integer(get_json_value_without_exception(<<"cur_exp">>, EquipLevel, 0)),
%%       EquipExp = dd_util:to_integer(get_json_value_without_exception(<<"exp">>, EquipItem, 0)),
%%       Exp =
%%         if
%%           EquipOldExp > EquipExp -> EquipOldExp;
%%           true -> EquipExp
%%         end,
%%       #equipment{id = dd_util:to_list(EquipId), no = dd_util:to_list(EquipNo), exp = Exp}
%%     end, EquipJsonList),
%%
%%   MaterialList = lists:map(
%%     fun({struct, MaterialItem}) ->
%%       MaterialId = get_json_value(<<"id">>, MaterialItem),
%%       MaterialCount = get_json_value(<<"count">>, MaterialItem),
%%       #material{id = dd_util:to_list(MaterialId), count = dd_util:to_integer(MaterialCount)}
%%     end, MaterialJsonList),

  PropTree = lists:foldl(
    fun({struct, PropItem}, TmpTree) ->
      PropertyId = dd_util:to_list(get_json_value(<<"id">>, PropItem)),
      PropertyCount = dd_util:to_integer(get_json_value(<<"count">>, PropItem)),
      case gb_trees:lookup(PropertyId, TmpTree) of
        none ->
          gb_trees:insert(PropertyId, PropertyCount, TmpTree);
        {value, _} -> TmpTree  %%存在重复的，不作处理
      end
    end, gb_trees:empty(), PropJsonList),

  EquipPieceTree = lists:foldl(
    fun({struct, PieceItem}, TmpTree) ->
      Id = dd_util:to_list(get_json_value(<<"id">>, PieceItem)),
      Count = dd_util:to_integer(get_json_value(<<"count">>, PieceItem)),
      case gb_trees:lookup(Id, TmpTree) of
        none ->
          gb_trees:insert(Id, Count, TmpTree);
        {value, _} -> TmpTree  %%存在重复的，不作处理
      end
    end, gb_trees:empty(), EquipPieceJsonList),

  InscriptionPieceTree = lists:foldl(
    fun({struct, PieceItem}, TmpTree) ->
      Id = dd_util:to_list(get_json_value(<<"id">>, PieceItem)),
      Count = dd_util:to_integer(get_json_value(<<"count">>, PieceItem)),
      case gb_trees:lookup(Id, TmpTree) of
        none ->
          gb_trees:insert(Id, Count, TmpTree);
        {value, _} -> TmpTree  %%存在重复的，不作处理
      end
    end, gb_trees:empty(), InscriptionPieceJsonList),

  InscriptionList = lists:map(
    fun({struct, InsItem}) ->
      Pos = get_json_value(<<"pos">>, InsItem),
      ID = get_json_value(<<"id">>, InsItem),
      #inscription{id = dd_util:to_list(ID), pos = dd_util:to_integer(Pos)}
    end, InscriptionJsonList),
  
  S_equipment_list = ss_d_item_list(S_equipment),
  S_material_list = ss_d_item_list(S_material),
  S_fragment_list = ss_d_item_list(S_fragment),
  S_consumables_list = ss_d_item_list(S_consumables),
  S_card_list = ss_d_item_list(S_card),


  #backpack{equipment_list = [], material_list = gb_trees:empty(), prop_list = PropTree, inscription_list = InscriptionList,
    inscription_piece_list = InscriptionPieceTree, equip_piece = EquipPieceTree, capacity = dd_util:to_integer(Capacity)
	%superstar属性
	,s_equipment_list = S_equipment_list
	,s_material_list = S_material_list
	,s_fragment_list = S_fragment_list
	,s_consumables_list = S_consumables_list
	,s_card_list = S_card_list
	}.

 %% #backpack{equipment_list = EquipList, material_list = MaterialList, prop_list = PropList, capacity = dd_util:to_integer(Capacity)}.

decode_reward_match(RMList) ->
	{struct, Json} = mochijson2:decode(RMList),
	
	Fee = get_json_value(<<"fee">>, Json),
	Fee_temp = get_json_value(<<"fee_temp">>, Json),
	Fee_battle = get_json_value(<<"fee_battle">>, Json),
	Combo = get_json_value(<<"combo">>, Json),
	Dis_slot = get_json_value(<<"dis_slot">>, Json),
	Level = get_json_value(<<"level">>, Json),
	Levelmax = get_json_value(<<"levelmax">>, Json),
	Hp = get_json_value(<<"hp">>, Json),
	Hpmax = get_json_value(<<"hpmax">>, Json),
	Status = get_json_value(<<"status">>, Json),
	BattleID = get_json_value(<<"battle_id">>, Json),
	SelectIndex = get_json_value(<<"select_index">>, Json),
	MaxIndex = get_json_value(<<"max_index">>, Json),
	MaxIndexBuff = get_json_value(<<"max_index_buff">>, Json),
	
	
	CardJsonList = get_json_value_without_exception(<<"card_list">>, Json, []),
	CardList =
    lists:map(
      fun({struct, CardItem}) ->
        Index = dd_util:to_integer(get_json_value(<<"index">>, CardItem)),
        Type = dd_util:to_integer(get_json_value(<<"type">>, CardItem)),
		ID = dd_util:to_integer(get_json_value(<<"id">>, CardItem)),
        #rm_card{index = Index, type = Type,id = ID}
      end, CardJsonList),
	
	CardHidJsonList = get_json_value_without_exception(<<"card_list_hid">>, Json, []),
	CardHidList =
    lists:map(
      fun({struct, CardItem}) ->
        Index = dd_util:to_integer(get_json_value(<<"index">>, CardItem)),
        Type = dd_util:to_integer(get_json_value(<<"type">>, CardItem)),
		ID = dd_util:to_integer(get_json_value(<<"id">>, CardItem)),
        #rm_card{index = Index, type = Type,id = ID}
      end, CardHidJsonList),
	
	BuffJsonList = get_json_value_without_exception(<<"buff_list">>, Json, []),
	BuffList = 
	lists:map(
		fun({struct, BuffItem}) ->
		Index = dd_util:to_integer(get_json_value(<<"index">>, BuffItem)),
        Type = dd_util:to_integer(get_json_value(<<"type">>, BuffItem)),
		ID = dd_util:to_integer(get_json_value(<<"id">>, BuffItem)),
        #rm_buff{index = Index,type = Type,id = ID}
      end, BuffJsonList),
	
	ComboJsonList = get_json_value_without_exception(<<"combomax">>, Json, []),
	ComboList = 
	lists:map(
		fun({struct, ComboItem}) ->
        Level = dd_util:to_integer(get_json_value(<<"level">>, ComboItem)),
		Max = dd_util:to_integer(get_json_value(<<"max">>, ComboItem)),
        #rm_combo{level = Level,max = Max}
      end, ComboJsonList),
	
	LevelCountJsonList = get_json_value_without_exception(<<"levelcount">>, Json, []),
	LevelCountList = 
	lists:map(
		fun({struct, ComboItem}) ->
        Level = dd_util:to_integer(get_json_value(<<"level">>, ComboItem)),
		Max = dd_util:to_integer(get_json_value(<<"max">>, ComboItem)),
        #rm_combo{level = Level,max = Max}
      end, LevelCountJsonList),
	
	RewardTeamJsonList = get_json_value_without_exception(<<"reward_team">>, Json, []),
	RewardTeamList = 
		lists:map(fun({struct, RewardItem})->
						  Pos = dd_util:to_integer(get_json_value(<<"pos">>, RewardItem)),
						  PlayerId = dd_util:to_integer(get_json_value(<<"id">>, RewardItem)),
						  {Pos,PlayerId}
				  end, RewardTeamJsonList),
	#reward_match{fee = Fee, 
				  fee_temp = Fee_temp,
				  fee_battle = Fee_battle,
				  combo = Combo,
				  dis_slot = Dis_slot,
				  level = Level,
				  levelmax = Levelmax,
				  hp = Hp,
				  hpmax = Hpmax,
				  status = Status, 
				  battle_id = BattleID,
				  select_index = SelectIndex,
				  max_index = MaxIndex,
				  max_index_buff = MaxIndexBuff,
				  card_list = CardList,
				  card_list_hid = CardHidList,
				  buff_list = BuffList,
				  combomax = ComboList,
				  levelcount = LevelCountList,
				  reward_team = RewardTeamList
				  }.

decode_ts_item(RMTSJson)->
	{struct, Json} = mochijson2:decode(RMTSJson),
	Count = get_json_value(<<"count">>, Json),
	Last_update_ts = get_json_value(<<"last_update_ts">>, Json),
	#ts_item{count = Count, last_update_ts = Last_update_ts}.

decode_mission_item(MissionItemJson) ->
  MissionID = dd_util:to_list(get_json_value_without_exception(<<"id">>, MissionItemJson, "")),
  MissionProgress1 = dd_util:to_integer(get_json_value_without_exception(<<"p1">>, MissionItemJson, 0)),
  MissionProgress2 = dd_util:to_integer(get_json_value_without_exception(<<"p2">>, MissionItemJson, 0)),
  MissionFinishTs = dd_util:to_integer(get_json_value_without_exception(<<"fts">>, MissionItemJson, 0)),
  MissionGetRewardTs = dd_util:to_integer(get_json_value_without_exception(<<"rts">>, MissionItemJson, 0)),
  RecordJsonList = get_json_value_without_exception(<<"rd">>, MissionItemJson, []),
  MissionType = dd_util:to_integer(get_json_value(<<"type">>, MissionItemJson)),
  MissionFinishTimes = dd_util:to_integer(get_json_value_without_exception(<<"fcnt">>, MissionItemJson, 0)),
  RecordList =
    lists:map(
      fun({struct, RecordItem}) ->
        Key = dd_util:to_list(get_json_value(<<"key">>, RecordItem)),
        Value = dd_util:to_integer(get_json_value(<<"val">>, RecordItem)),
        #record_item{key = Key, value = Value}
      end, RecordJsonList),
  #mission_item{mission_id = MissionID, mission_progress_1 = MissionProgress1, mission_progress_2 = MissionProgress2, mission_finish_ts = MissionFinishTs, mission_get_reward_ts = MissionGetRewardTs, mission_record = RecordList, mission_type = MissionType, mission_finish_times = MissionFinishTimes}.

decode_player_activity(PlayerActivity) ->
  ActivityValue = dd_util:to_integer(get_json_value_without_exception(<<"val">>, PlayerActivity, 0)),
  RewardJsonList = get_json_value_without_exception(<<"al">>, PlayerActivity, []),
  FinishList =
    lists:map(
      fun({struct, RewardItem}) ->
        ID = dd_util:to_integer(get_json_value(<<"id">>, RewardItem)),
        FinishTs = dd_util:to_integer(get_json_value(<<"fts">>, RewardItem)),
        GetRewardTs = dd_util:to_integer(get_json_value(<<"rts">>, RewardItem)),
        #activity_reward_item{id = ID, finish_ts = FinishTs, get_reward_ts = GetRewardTs}
      end, RewardJsonList),
  #player_activity{activity_value = ActivityValue, activity_reward_list = FinishList}.

decode_mission(Mission) ->
  {struct, JsonList} = mochijson2:decode(Mission),
  MissionJsonList = get_json_value_without_exception(<<"ml">>, JsonList, []),
  {struct, ActivityJson} = get_json_value_without_exception(<<"ac">>, JsonList, {struct, []}),
  PlayerActivity = decode_player_activity(ActivityJson),
  MissionList = lists:map(fun({struct, MissionItem}) -> decode_mission_item(MissionItem) end, MissionJsonList),
  #mission{mission_list = MissionList, player_activity = PlayerActivity}.
%%%%%%%%%%%%%%%%%%%%%%%%%

decode_achievement_item(AchievementItem) ->
  ID = dd_util:to_list(get_json_value(<<"id">>, AchievementItem)),
  Statistics = dd_util:to_integer(get_json_value(<<"s1">>, AchievementItem)),
  Statistics1 = dd_util:to_integer(get_json_value(<<"s2">>, AchievementItem)),
  RecordJsonList = get_json_value(<<"rd">>, AchievementItem),
  FinishTs = dd_util:to_integer(get_json_value(<<"fts">>, AchievementItem)),
  GetRewardTs = dd_util:to_integer(get_json_value(<<"rts">>, AchievementItem)),
  Progress = dd_util:to_integer(get_json_value(<<"pv">>, AchievementItem)),
  RecordList =
    lists:map(
      fun({struct, RecordItem}) ->
        RecordID = dd_util:to_list(get_json_value(<<"key">>, RecordItem)),
        RecordValue = dd_util:to_integer(get_json_value(<<"val">>, RecordItem)),
        #record_item{key = RecordID, value = RecordValue}
      end, RecordJsonList),
  #achievement_item{id = ID, statistics = Statistics, statistics_1 = Statistics1, record = RecordList, finish_ts = FinishTs, get_reward_ts = GetRewardTs, progress = Progress}.

decode_achievement(Achievement) ->
  {struct, JsonList} = mochijson2:decode(Achievement),
  JsonGroupList = get_json_value_without_exception(<<"gl">>, JsonList, []),
  LastUpdateTs = dd_util:to_integer(get_json_value_without_exception(<<"uts">>, JsonList, 0)),
  AchievementGroupList =
    lists:map(
      fun({struct, GroupItem}) ->
        GroupId = dd_util:to_integer(get_json_value(<<"id">>, GroupItem)),
        {struct, ItemJson1} = get_json_value(<<"1">>, GroupItem),
        {struct, ItemJson2} = get_json_value(<<"2">>, GroupItem),
        {struct, ItemJson3} = get_json_value(<<"3">>, GroupItem),
        #achievement_group{group_id = GroupId, item_level_1 = decode_achievement_item(ItemJson1), item_level_2 = decode_achievement_item(ItemJson2), item_level_3 = decode_achievement_item(ItemJson3)}
      end, JsonGroupList),
  #achievement{group_list = AchievementGroupList, last_update_ts = LastUpdateTs}.

decode_shop(Shop) ->
  {struct, JsonList} = mochijson2:decode(Shop),
  GoodJsonList = get_json_value_without_exception(<<"gl">>, JsonList, []),
  PermanentGoodsJsonList = get_json_value_without_exception(<<"pgl">>, JsonList, []),
  LastUpdateTs = get_json_value_without_exception(<<"uts">>, JsonList, dd_util:timestamp()),
  {struct,PayInfoJsonList} = get_json_value_without_exception(<<"pi">>, JsonList, {struct, []}),
  GoodList =
    lists:map(
      fun({struct, ItemList}) ->
        ID = dd_util:to_list(get_json_value(<<"id">>, ItemList)),
        RemainNum = dd_util:to_integer(get_json_value(<<"rcnt">>, ItemList)),
        LatestUpdateTs = dd_util:to_integer(get_json_value(<<"bts">>, ItemList)),
        #goods{id = ID, remain_count = RemainNum, latest_buy_ts = LatestUpdateTs}
      end, GoodJsonList),
  PermanentGoodsList =
    lists:map(
      fun({struct, ItemList}) ->
        ID = dd_util:to_list(get_json_value(<<"id">>, ItemList)),
        RemainNum = dd_util:to_integer(get_json_value(<<"rcnt">>, ItemList)),
        LatestUpdateTs = dd_util:to_integer(get_json_value(<<"bts">>, ItemList)),
        #goods{id = ID, remain_count = RemainNum, latest_buy_ts = LatestUpdateTs}
      end, PermanentGoodsJsonList),
  FirstPayTs = dd_util:to_integer(get_json_value_without_exception(<<"fp">>, PayInfoJsonList, 0)),
  TotalGem = dd_util:to_integer(get_json_value_without_exception(<<"tg">>, PayInfoJsonList, 0)),
  TotalPay = dd_util:list_to_float(dd_util:to_list(get_json_value_without_exception(<<"tp">>, PayInfoJsonList, "0.0")), 0.0),
  RecordJsonList = get_json_value_without_exception(<<"pr">>, PayInfoJsonList, []),
  Record =
    lists:map(
      fun({struct, Item}) ->
        Ts = dd_util:to_integer(get_json_value(<<"ts">>, Item)),
        ID = dd_util:to_list(get_json_value(<<"id">>, Item)),
        Gem = dd_util:to_integer(get_json_value(<<"gem">>, Item)),
        Price = dd_util:list_to_float(dd_util:to_list(get_json_value(<<"pp">>, Item)), 0.0),
        #pay_item{pay_ts = Ts, pay_gem_count = Gem, pay_price = Price, pay_item_id = ID}
      end, RecordJsonList),
  PayInfo = #pay_info{first_pay_ts = FirstPayTs, pay_record = Record, total_gem_count = TotalGem, total_pay_val = TotalPay},
  #shop{goods_list = GoodList, permanent_buy_list = PermanentGoodsList, last_update_ts = LastUpdateTs, pay_info = PayInfo}.

decode_stage(Stage) ->
  {struct, StageList} = mochijson2:decode(Stage),
  BaseTollgateJsonList = get_json_value_without_exception(<<"bt">>, StageList, []),
  ActivityTollgateJson = get_json_value_without_exception(<<"at">>, StageList, []),
  {struct, EndlessTollgateJson} = get_json_value_without_exception(<<"et">>, StageList, {struct, []}),
  HarvestObstacleJson = get_json_value_without_exception(<<"ho">>, StageList, []),
  {struct, TollgateDropJson} = get_json_value_without_exception(<<"td">>, StageList, {struct, []}),
  BaseTollgateList = lists:map(
    fun({struct, Tollgate}) ->
      ID = dd_util:to_integer(get_json_value(<<"id">>, Tollgate)),
      MaxScore = dd_util:to_integer(get_json_value(<<"msc">>, Tollgate)),
      MaxStar = dd_util:to_integer(get_json_value(<<"mst">>, Tollgate)),
      RemainTimes = dd_util:to_integer(get_json_value_without_exception(<<"rcnt">>, Tollgate, 0)),
      LastPassTs = dd_util:to_integer(get_json_value_without_exception(<<"pts">>, Tollgate, dd_util:timestamp())),
      MaxStarSeq = dd_util:to_list(get_json_value_without_exception(<<"msts">>, Tollgate, get_max_star_seq(MaxStar))),
      AdditionRemain = dd_util:to_integer(get_json_value_without_exception(<<"arcnt">>, Tollgate, 0)),
      #tollgate{id = ID, max_star = MaxStar, max_score = MaxScore, max_star_seq = MaxStarSeq, daily_remain_times = RemainTimes, addition_remain_times = AdditionRemain, last_pass_ts = LastPassTs}
    end, BaseTollgateJsonList),

  HarvestObstacleList = lists:map(
    fun({struct, HarvestItem}) ->
      ID = get_json_value(<<"id">>, HarvestItem),
      RemainNum = get_json_value(<<"rcnt">>, HarvestItem),
      LastUpdateTs = get_json_value(<<"uts">>, HarvestItem),
      #harvest_obstacles_item{id = dd_util:to_list(ID), remain_number = dd_util:to_integer(RemainNum), last_update_ts = dd_util:to_integer(LastUpdateTs)}
    end, HarvestObstacleJson),

  ActivityTollgate =
    lists:map(
      fun({struct, Item}) ->
        Id = dd_util:to_integer(get_json_value(<<"id">>, Item)),
        MScore = dd_util:to_integer(get_json_value(<<"msc">>, Item)),
        MStar = dd_util:to_integer(get_json_value(<<"mst">>, Item)),
        MStarS = dd_util:to_list(get_json_value_without_exception(<<"msts">>, Item, get_max_star_seq(MStar))),
        RemainTimes = dd_util:to_integer(get_json_value_without_exception(<<"rcnt">>, Item, 0)),
        LastFinishTs = dd_util:to_integer(get_json_value(<<"fts">>, Item)),
        #activity_tollgate_item{tollgate_id = Id, max_score = MScore, max_star = MStar, max_star_seq = MStarS, daily_remain_times = RemainTimes, last_finish_ts = LastFinishTs}
      end, ActivityTollgateJson),

 %%解析无尽数据
  EndlessMaxScore = dd_util:to_integer(get_json_value_without_exception(<<"msc">>, EndlessTollgateJson, 0)),
  MaxGainTs = dd_util:to_integer(get_json_value_without_exception(<<"mgts">>, EndlessTollgateJson, 0)),
  MaxWaveCount = dd_util:to_integer(get_json_value_without_exception(<<"mwc">>, EndlessTollgateJson, 0)),
  LastWeekMaxScore = dd_util:to_integer(get_json_value_without_exception(<<"lwsc">>, EndlessTollgateJson, 0)),
  LastGainTs = dd_util:to_integer(get_json_value_without_exception(<<"lgts">>, EndlessTollgateJson, 0)),
  ThisWeekMaxScore = dd_util:to_integer(get_json_value_without_exception(<<"twsc">>, EndlessTollgateJson, 0)),
  ThisGainTs = dd_util:to_integer(get_json_value_without_exception(<<"tgts">>, EndlessTollgateJson, 0)),
  WeekRecordJson = get_json_value_without_exception(<<"wrdl">>, EndlessTollgateJson, []),
  EndlessLastUpdateTs = get_json_value_without_exception(<<"uts">>, EndlessTollgateJson, dd_util:timestamp()),
  {struct, DropJson} = get_json_value_without_exception(<<"ed">>, EndlessTollgateJson, {struct, []}),
  SeedVal = dd_util:to_integer(get_json_value_without_exception(<<"sval">>, DropJson, 0)),
  RandVal = dd_util:to_integer(dd_util:to_list(get_json_value_without_exception(<<"rval">>, DropJson, ?RAND_NUMBER))),
  SeedTs = dd_util:to_integer(get_json_value_without_exception(<<"sts">>, DropJson, 0)),
  EndTs = dd_util:to_integer(get_json_value_without_exception(<<"ets">>, DropJson, 0)),
  DropJsonList = get_json_value_without_exception(<<"dl">>, DropJson, []),

  DropList =
    lists:map(
      fun({struct, Item}) ->
        ChestId = dd_util:to_integer(get_json_value(<<"id">>, Item)),
        SubId = dd_util:to_integer(get_json_value(<<"sid">>, Item)),
        Id = dd_util:to_list(get_json_value(<<"did">>, Item)),
        Type = dd_util:to_integer(get_json_value(<<"type">>, Item)),
        Count = dd_util:to_integer(get_json_value(<<"cnt">>, Item)),
        Level = dd_util:to_integer(get_json_value(<<"lvl">>, Item)),
        #treasure_item{treasure_id = ChestId, sub_id = SubId, drop_id = Id, drop_type = Type, count = Count, drop_level = Level}
      end, DropJsonList),
  Drop = #endless_drop{seed_ts = SeedTs, end_ts = EndTs, seed_val = SeedVal, rand_val = RandVal, endless_drop = DropList},

  %%解析关卡掉落数据
  TSeedVal = dd_util:to_integer(get_json_value_without_exception(<<"sval">>, TollgateDropJson, 0)),
  TRandVal = dd_util:to_integer(dd_util:to_list(get_json_value_without_exception(<<"rval">>, TollgateDropJson, ?RAND_NUMBER))),
  TSeedTs = dd_util:to_integer(get_json_value_without_exception(<<"sts">>, TollgateDropJson, 0)),
  TEndTs = dd_util:to_integer(get_json_value_without_exception(<<"ets">>, TollgateDropJson, 0)),
  TollgateDrop = #tollgate_drop{seed_val = TSeedVal, rand_val = TRandVal, seed_ts = TSeedTs, end_ts = TEndTs},

  WeekRecord =
    lists:map(
      fun({struct, Item}) ->
        Time = dd_util:to_integer(get_json_value(<<"time">>, Item)),
        RecordScore = dd_util:to_integer(get_json_value(<<"score">>, Item)),
        TollgateID = dd_util:to_integer(get_json_value(<<"tid">>, Item)),
        #endless_item{time = Time, score = RecordScore, tollgate_id = TollgateID}
      end, WeekRecordJson),
  Endless = #endless_tollgate{max_score = EndlessMaxScore, max_score_gain_ts = MaxGainTs, max_wave_count = MaxWaveCount, last_week_max_score = LastWeekMaxScore, last_week_max_score_gain_ts = LastGainTs, this_week_max_score = ThisWeekMaxScore, this_week_max_score_gain_ts = ThisGainTs, week_score_record = WeekRecord, last_update_ts = EndlessLastUpdateTs, endless_drop = Drop},
  #stage{base_tollgate_list = BaseTollgateList, tollgate_drop = TollgateDrop, ac_tollgate = ActivityTollgate, harvest_obstacles_list = HarvestObstacleList, endless_tollgate = Endless}.

decode_login_reward(LoginReward) ->
  {struct, LoginRewardList} = mochijson2:decode(LoginReward),
  LatestLoginTs = get_json_value_without_exception(<<"llts">>, LoginRewardList, dd_util:timestamp()),
  LoginTimes = get_json_value_without_exception(<<"lcnt">>, LoginRewardList, 1),
  RewardList = get_json_value_without_exception(<<"rl">>, LoginRewardList, []),
  TotalLoginDays = get_json_value_without_exception(<<"td">>, LoginRewardList, 0),
  List = lists:map(
    fun({struct, RewardItem}) ->
      Id = get_json_value(<<"id">>, RewardItem),
      LoginTs = get_json_value(<<"lts">>, RewardItem),
      #login_reward_item{id = dd_util:to_integer(Id), login_ts = dd_util:to_integer(LoginTs)}
    end, RewardList),
  #login_reward{latest_login_ts = dd_util:to_integer(LatestLoginTs), login_times = dd_util:to_integer(LoginTimes), login_reward_list = List, total_login_days = dd_util:to_integer(dd_util:to_list(TotalLoginDays))}.

decode_strength(Strength) ->
  {struct, StrengthL} = mochijson2:decode(Strength),
  StrengthV = dd_util:to_integer(get_json_value_without_exception(<<"sval">>, StrengthL, 100)),
  LastUpdateTs = dd_util:to_integer(get_json_value_without_exception(<<"uts">>, StrengthL, dd_util:timestamp())),
  GiftClose = dd_util:to_integer(get_json_value_without_exception(<<"gcls">>, StrengthL, 1)),
  BuyTimes = dd_util:to_integer(get_json_value_without_exception(<<"bcnt">>, StrengthL, 0)),
  GiveStrengthJson = get_json_value_without_exception(<<"gsth">>, StrengthL, []),
  GiveStrength =
    lists:map(
      fun({struct, Item}) ->
        Value = dd_util:to_integer(get_json_value(<<"val">>, Item)),
        Source = dd_util:to_integer(get_json_value(<<"src">>, Item)),
        Dest = dd_util:to_integer(get_json_value(<<"dst">>, Item)),
        Ts = dd_util:to_integer(get_json_value(<<"ts">>, Item)),
        #strength_item{give_strength_value = Value, give_strength_source = Source, give_strength_dest = Dest, give_strength_ts = Ts}
      end, GiveStrengthJson),
  #strength{strength = StrengthV, last_update_ts = LastUpdateTs, close_strength_gift = GiftClose, give_friend_strength = GiveStrength, today_buy_times = BuyTimes}.

decode_lottery(Lottery) ->
  {struct, LotteryJsonList} = mochijson2:decode(Lottery),
  JsonList = get_json_value_without_exception(<<"ll">>, LotteryJsonList, []),
  Ts = dd_util:to_integer(get_json_value_without_exception(<<"ts">>, LotteryJsonList, 0)),
  Single = dd_util:to_integer(get_json_value_without_exception(<<"slt">>, LotteryJsonList, 0)),
  Ten = dd_util:to_integer(get_json_value_without_exception(<<"tlt">>, LotteryJsonList, 0)),
  ItemList =
    lists:map(
      fun({struct, Item}) ->
        ChestId = dd_util:to_integer(get_json_value(<<"id">>, Item)),
        SubId = dd_util:to_integer(get_json_value(<<"sid">>, Item)),
        Id = dd_util:to_list(get_json_value(<<"did">>, Item)),
        Type = dd_util:to_integer(get_json_value(<<"type">>, Item)),
        Count = dd_util:to_integer(get_json_value(<<"cnt">>, Item)),
        Level = dd_util:to_integer(get_json_value_without_exception(<<"lvl">>, Item, 1)),
        #treasure_item{treasure_id = ChestId, sub_id = SubId, drop_id = Id, drop_type = Type, count = Count, drop_level = Level}
      end, JsonList),
  #lottery{today_lottery_lists = ItemList, last_lottery_ts = Ts, single_lottery_times = Single, ten_lottery_times = Ten}.

decode_guild(_Guild) ->
  #guild{}.

decode_competitive(_Competitive) ->
  #competitive{}.

decode_addition(Addition) ->
  {struct, AdditionJsonList} = mochijson2:decode(Addition),
  NewGuideJsonList = get_json_value_without_exception(<<"ngs">>, AdditionJsonList, []),
  OldGuidStep = dd_util:to_integer(get_json_value_without_exception(<<"gs">>, AdditionJsonList, 1000)),
  RankRewardGot = dd_util:to_integer(get_json_value_without_exception(<<"rrg">>, AdditionJsonList, 0)),
  PackJsonList = get_json_value_without_exception(<<"pl">>, AdditionJsonList, []),
  NewGuide =
    lists:map(
      fun({struct,ItemJson}) ->
        Key = dd_util:to_list(get_json_value(<<"k">>, ItemJson)),
        Val = dd_util:to_integer(get_json_value(<<"v">>, ItemJson)),
        #record_item{key = Key, value = Val}
      end, NewGuideJsonList),
  PackList =
    lists:foldl(
      fun({struct, PackItemJson}, TmpTree) ->
        Key = dd_util:to_list(get_json_value(<<"k">>, PackItemJson)),
        Val = dd_util:to_integer(get_json_value(<<"v">>, PackItemJson)),
        gb_trees:insert(Key, Val, TmpTree)
      end, gb_trees:empty(), PackJsonList),
  #addition{newer_guide_steps = OldGuidStep, rank_reward_got = RankRewardGot, guide_steps = NewGuide, cdkey_pack_list = PackList}.

decode_create_ts(CreateTs) ->
  dd_util:to_integer(CreateTs).

decode_last_login_ts(LastLoginTs) ->
  dd_util:to_integer(LastLoginTs).

get_max_star_seq(Star) when is_integer(Star) ->
  case Star of
    0 -> [-1, -1, -1];
    1 -> [0, -1, -1];
    2 -> [0, 0, -1];
    3 -> [0, 0, 0]
  end.

get_json_value(Key, PropList) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      throw({custom, "error json key" ++ dd_util:to_list(Key)});
    Value -> Value
  end.

get_json_value_without_exception(Key, PropList, DefaultValue) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined -> DefaultValue;
    Value -> Value
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_mail_template_rd([TemplateID, TemplateType, TemplateTag, TemplateTitle, TemplateContent, ParamLen]) ->
   #template_mail
   {
      template_id = decode_template_id(TemplateID),
      template_type = decode_template_type(TemplateType),
      template_tag = decode_template_tag(TemplateTag),
      template_title = decode_template_title(TemplateTitle),
      template_content = decode_template_content(TemplateContent),
      template_content_parm_len = decode_template_param_len(ParamLen)
   }.

decode_bulletin_mail_rd([MailID, MailTemplateID, MailParam, MailAddTime, MailTerm]) ->
  #bulletin_mail
  {
      mail_id = decode_mail_id(MailID),
      mail_template_id = decode_template_id(MailTemplateID),
      mail_param_list = decode_mail_param(MailParam),
      mail_add_ts = decode_mail_add_ts(MailAddTime),
      mail_term = decode_mail_term(MailTerm)
  }.

decode_attach_mail_rd([MailID, MailSource, MailDest, MailTemplateID, MailParam, MailAttachment, MailAddTs, MailType, MailTerm]) ->
  #attach_mail
  {
    mail_id = decode_mail_id(MailID),
    mail_source = decode_mail_source(MailSource),
    mail_dest = decode_mail_dest(MailDest),
    mail_template_id = decode_template_id(MailTemplateID),
    mail_param_list = decode_mail_param(MailParam),
    mail_attachment = decode_mail_attachment(MailAttachment),
    mail_add_ts = decode_mail_add_ts(MailAddTs),
    mail_type = decode_mail_type(MailType),
    mail_term = decode_mail_term(MailTerm)
  }.

decode_mail_id(MailID) -> dd_util:to_integer(MailID).
decode_mail_param(Param) ->
  {struct, ParamJson} = mochijson2:decode(dd_util:to_list(Param)),
  ParamJsonList = get_json_value(<<"param">>, ParamJson),
  lists:map( fun(Item) -> dd_util:to_list(Item) end, ParamJsonList).

decode_mail_add_ts(Ts) -> dd_util:to_integer(Ts).
decode_mail_term(Term) -> dd_util:to_integer(Term).
decode_mail_source(Source) -> dd_util:to_integer(Source).
decode_mail_dest(Dest) -> dd_util:to_integer(Dest).
decode_mail_attachment(Attachment) ->
  {struct, AttachmentJsonList} = mochijson2:decode(Attachment),
  Type = dd_util:to_integer(get_json_value(<<"type">>, AttachmentJsonList)),
  ID = dd_util:to_list(get_json_value(<<"property_id">>, AttachmentJsonList)),
  Count = dd_util:to_integer(get_json_value(<<"count">>, AttachmentJsonList)),
  #mail_attachment{property_id = ID, type = Type, count = Count}.

decode_mail_type(Type) -> dd_util:to_integer(Type).

decode_template_id(TemplateID) -> dd_util:to_list(TemplateID).
decode_template_type(TemplateType) -> dd_util:to_integer(TemplateType).
decode_template_tag(TemplateTag) -> dd_util:to_list(TemplateTag).
decode_template_title(TemplateTitle) -> dd_util:to_list(TemplateTitle).
decode_template_content(TemplateContent) -> dd_util:to_list(TemplateContent).
decode_template_param_len(ParamLen) -> dd_util:to_integer(ParamLen).


encode_mail_id(MailID) when is_integer(MailID) -> dd_util:to_list(MailID).
encode_mail_param(ParamList) when is_list(ParamList) ->
  %%JsonList = lists:map(fun(Item) -> dd_util:to_binary(Item) end, ParamList),
  Json = {struct,[{<<"param">>, ParamList}]},
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(dd_util:to_list(Value)).


encode_mail_add_ts(Ts) when is_integer(Ts) -> dd_util:to_list(Ts).
encode_mail_term(Term) when is_integer(Term) -> dd_util:to_list(Term).
encode_mail_source(Source) when is_integer(Source) -> dd_util:to_list(Source).
encode_mail_dest(Dest) when is_integer(Dest) -> dd_util:to_list(Dest).
encode_mail_attachment(Attachment) when is_record(Attachment, mail_attachment) ->
  Json =
    {
      struct,
      [
        {<<"type">>, Attachment#mail_attachment.type},
        {<<"property_id">>, dd_util:to_binary(Attachment#mail_attachment.property_id)},
        {<<"count">>, Attachment#mail_attachment.count}
      ]
    },
  Value = dd_util:encode_json_utf8(Json),
  mysql_util:escape(Value).

encode_mail_type(Type) when is_integer(Type) -> dd_util:to_list(Type).

encode_template_id(TemplateID) when is_list(TemplateID) -> dd_util:to_binary(TemplateID).
encode_template_type(TemplateType) when is_integer(TemplateType) -> dd_util:to_list(TemplateType).
encode_template_tag(TemplateTag) when is_list(TemplateTag) -> dd_util:to_binary(TemplateTag).
encode_template_title(TemplateTitle) when is_list(TemplateTitle) -> dd_util:to_binary(TemplateTitle).
encode_template_content(TemplateContent) when is_list(TemplateContent) -> dd_util:to_binary(TemplateContent).
encode_template_param_len(ParamLen) when is_integer(ParamLen) -> dd_util:to_list(ParamLen).


get_create_mail_template_sql(MailTemplate) when is_record(MailTemplate, template_mail) ->
  mysql_util:insert_query(
    "mail_template",
    ["template_id", "template_type", "template_tag", "template_title", "template_content", "template_content_param_len"],
    [
      encode_template_id(MailTemplate#template_mail.template_id),
      encode_template_type(MailTemplate#template_mail.template_type),
      encode_template_tag(MailTemplate#template_mail.template_tag),
      encode_template_title(MailTemplate#template_mail.template_title),
      encode_template_content(MailTemplate#template_mail.template_content),
      encode_template_param_len(MailTemplate#template_mail.template_content_parm_len)
    ]
  ).

get_create_attach_mail_sql(AttachMail) when is_record(AttachMail, attach_mail) ->
  mysql_util:insert_query(
    "attach_mail",
    ["mail_id", "mail_source", "mail_dest", "mail_template_id", "mail_param", "mail_attachment", "mail_add_time", "mail_type", "mail_term"],
    [
      encode_mail_id(AttachMail#attach_mail.mail_id),
      encode_mail_source(AttachMail#attach_mail.mail_source),
      encode_mail_dest(AttachMail#attach_mail.mail_dest),
      encode_template_id(AttachMail#attach_mail.mail_template_id),
      encode_mail_param(AttachMail#attach_mail.mail_param_list),
      encode_mail_attachment(AttachMail#attach_mail.mail_attachment),
      encode_mail_add_ts(AttachMail#attach_mail.mail_add_ts),
      encode_mail_type(AttachMail#attach_mail.mail_type),
      encode_mail_term(AttachMail#attach_mail.mail_term)
    ]
  ).

get_create_bulletin_mail_sql(Bulletin) when is_record(Bulletin, bulletin_mail) ->
  mysql_util:insert_query(
    "bulletin_mail",
    ["mail_id", "mail_template_id", "mail_param", "mail_add_time", "mail_term"],
    [
      encode_mail_id(Bulletin#bulletin_mail.mail_id),
      encode_template_id(Bulletin#bulletin_mail.mail_template_id),
      encode_mail_param(Bulletin#bulletin_mail.mail_param_list),
      encode_mail_add_ts(Bulletin#bulletin_mail.mail_add_ts),
      encode_mail_term(Bulletin#bulletin_mail.mail_term)
    ]
  ).

get_create_good_sql(GoodItem) when is_record(GoodItem, res_goods) ->
  mysql_util:insert_query("shop_config", ["id","name","image","goods_id","goods_type","goods_count","cost_type",
    "cost_count","gift_type","gift_count","restriction","recommended","start_time","over_time"],
    [
      encode_shop_item_id(GoodItem#res_goods.id),
      encode_good_name(GoodItem#res_goods.name),
      encode_good_image(GoodItem#res_goods.pic),
      encode_good_id(GoodItem#res_goods.goods_no),
      encode_good_type(GoodItem#res_goods.goods_type),
      encode_good_count(GoodItem#res_goods.goods_count),
      encode_cost_type(GoodItem#res_goods.money_type),
      encode_cost_count(GoodItem#res_goods.money_count),
      encode_gift_type(GoodItem#res_goods.gift_type),
      encode_gift_count(GoodItem#res_goods.gift_count),
      encode_restriction(GoodItem#res_goods.restrict_count),
      encode_recommended(GoodItem#res_goods.is_recommend),
      encode_start_ts(GoodItem#res_goods.start_ts),
      encode_over_ts(GoodItem#res_goods.over_ts)
    ]).

decode_shop_config_item([ID, Name, Icon, GoodID, GoodType, GoodCount, CostType, CostCount, GiftType, GiftCount, Restriction, Recommend, StartTs, OverTs]) ->
  #res_goods
  {
    id = decode_shop_item_id(ID),
    name = decode_good_name(Name),
    pic = decode_good_image(Icon),
    goods_no = decode_good_id(GoodID),
    goods_type = decode_good_type(GoodType),
    goods_count = decode_good_count(GoodCount),
    money_type = decode_cost_type(CostType),
    money_count = decode_cost_count(CostCount),
    gift_type = decode_gift_type(GiftType),
    gift_count = decode_gift_count(GiftCount),
    restrict_count = decode_restriction(Restriction),
    is_recommend = decode_recommended(Recommend),
    start_ts = decode_start_ts(StartTs),
    over_ts = decode_over_ts(OverTs)
  }.

decode_lottery_config_item([ID, Tool_id, Price, Is_discount, StartTs, OverTs]) ->
  #lottery_conf
  {
    id = decode_lottery_conf_id(ID),
    tool_id = decode_lottery_conf_tool_id(Tool_id),
    price = decode_lottery_conf_price(Price),
    is_discount = decode_lottery_conf_discount(Is_discount),
    start_ts = decode_lottery_conf_starttm(StartTs),
    over_ts = decode_lottery_conf_stoptm(OverTs)
  }.

decode_lottery_conf_id(ID)->dd_util:to_integer(ID).
decode_lottery_conf_tool_id(T)->dd_util:to_list(T).
decode_lottery_conf_price(P)->dd_util:to_integer(P).
decode_lottery_conf_discount(D)->dd_util:to_integer(D).
decode_lottery_conf_starttm(S)->dd_util:to_integer(S).
decode_lottery_conf_stoptm(S)->dd_util:to_integer(S).



encode_shop_item_id(ID) when is_list(ID) -> ID.
encode_good_name(Name) when is_list(Name) -> mysql_util:escape(Name).
encode_good_image(Image) when is_list(Image) -> mysql_util:escape(Image).
encode_good_id(ID) when is_list(ID) -> ID.
encode_good_type(Type) -> dd_util:to_list(Type).
encode_good_count(Count) -> dd_util:to_list(Count).
encode_cost_type(Type) -> dd_util:to_list(Type).
encode_cost_count(Count) -> dd_util:to_list(Count).
encode_gift_type(Type) -> dd_util:to_list(Type).
encode_gift_count(Count) -> dd_util:to_list(Count).
encode_restriction(R) -> dd_util:to_list(R).
encode_recommended(R) -> dd_util:to_list(R).
encode_start_ts(R) -> dd_util:to_list(R).
encode_over_ts(R) -> dd_util:to_list(R).


decode_shop_item_id(ID) -> dd_util:to_list(ID).
decode_good_name(Name) -> dd_util:to_list(Name).
decode_good_image(Image)  -> dd_util:to_list(Image).
decode_good_id(ID) -> dd_util:to_list(ID).
decode_good_type(Type) -> dd_util:to_integer(Type).
decode_good_count(Count) -> dd_util:to_integer(Count).
decode_cost_type(Type) -> dd_util:to_integer(Type).
decode_cost_count(Count) -> dd_util:to_list(Count).
decode_gift_type(Type) -> dd_util:to_integer(Type).
decode_gift_count(Count) -> dd_util:to_integer(Count).
decode_restriction(R) -> dd_util:to_integer(R).
decode_recommended(R) -> dd_util:to_integer(R).
decode_start_ts(R) -> dd_util:to_integer(R).
decode_over_ts(R) -> dd_util:to_integer(R).


decode_notice_rd([Id, Title, Date, Detail, Pic_url, Sign]) ->
  #notice_rd{
    notice_id = dd_util:to_integer(Id),
    notice_title = dd_util:to_list(Title),
    notice_date = dd_util:to_list(Date),
    notice_detail = dd_util:to_list(Detail),
    notice_pic_url = dd_util:to_list(Pic_url),
    notice_sign = dd_util:to_list(Sign)
  }.

create_notice_sql(Notice) when is_record(Notice, notice_rd) ->
  mysql_util:insert_query("notice_rd", ["notice_title", "notice_date", "notice_detail", "notice_pic_url", "notice_sign"],
    [
      mysql_util:escape(dd_util:to_list(Notice#notice_rd.notice_title)),
      mysql_util:escape(dd_util:to_list(Notice#notice_rd.notice_date)),
      mysql_util:escape(dd_util:to_list(Notice#notice_rd.notice_detail)),
      mysql_util:escape(dd_util:to_list(Notice#notice_rd.notice_pic_url)),
      mysql_util:escape(dd_util:to_list(Notice#notice_rd.notice_sign))
    ]).

update_notice_sql(Notice) when is_record(Notice, notice_rd) ->
  mysql_util:update_query("notice_rd", ["notice_title", "notice_date", "notice_detail", "notice_pic_url", "notice_sign"],
    [
      mysql_util:escape(dd_util:to_list(Notice#notice_rd.notice_title)),
      mysql_util:escape(dd_util:to_list(Notice#notice_rd.notice_date)),
      mysql_util:escape(dd_util:to_list(Notice#notice_rd.notice_detail)),
      mysql_util:escape(dd_util:to_list(Notice#notice_rd.notice_pic_url)),
      mysql_util:escape(dd_util:to_list(Notice#notice_rd.notice_sign))
    ],
    "notice_id = " ++ dd_util:to_list(Notice#notice_rd.notice_id)).