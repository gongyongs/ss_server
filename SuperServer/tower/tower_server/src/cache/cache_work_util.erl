%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 一月 2015 下午4:29
%%%-------------------------------------------------------------------
-module(cache_work_util).
-author("zqlt").

-include("../../deps/file_log/include/file_log.hrl").
-include("cache_def.hrl").


%% API
-export([
  encode_ss_backpack/1, 
  encode_ss_rewardmatch/1,
  encode_super_team/1,
  encode_login_data/1,
  encode_user_info/1,
  encode_game_end/3,
  encode_advance_inscription/2,
  encode_buy_goods/2,
  encode_compose_inscription/1,
  encode_enter_tollgate/2,
  encode_exchange_cdkey/2,
  encode_fast_purchase/1,
  encode_login_reward/1,
  encode_mission_reward/2,
  encode_activity_reward/1,
  encode_achievement_reward/2,
  encode_user_basic_info/1,
  encode_endless_germ/1,
  encode_world_map_block/3,
  encode_equipment_strengthen/4,
  encode_replace_inscription/2,
  encode_update_tower_team/1,
  encode_friend_endless_rank/1,
  encode_server_endless_rank/1,
  encode_mass_attach_mail/3,
  encode_attach_mail/3,
  encode_iap_buy/2,
  encode_lottery/4,
  encode_present_friend_strength/3,
  encode_share_score/2,
  encode_platform_pay/2,
  encode_tollgate_sweep/2
]).

-export([
  get_player_basic_data/1
]).

%%考虑到节点之间的通信数据格式问题，将原来之间传输原生语言结构
%%改为传输json格式的二进制数据，以提高网络传输速度
%%
%%所有网络接口的返回值json二进制化对应一个接口

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%登录及查询相关数据%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%登陆数据返回，返回数据为{success， Data}
%%Data为二进制数据
encode_login_data(Account) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"login_reward">>, cache_work_json:encode_json_login_reward(Account#account.login_reward)},
        {<<"tower">>, cache_work_json:encode_json_tower(Account#account.heros)},
        {<<"strength_buy_times">>, Account#account.strength#strength.today_buy_times}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%get_user_info接口返回值
%%返回值为二进制数据
encode_user_info(UserValueList) when is_list(UserValueList) ->
  JsonList =
    lists:map(
      fun(ValueItem) ->
        case ValueItem of
          {"player", PlayerBasicData} ->
            PlayerValue = cache_work_json:encode_json_player(PlayerBasicData),
            {<<"player">>, PlayerValue};
		  {"team",SuperTeam}->
			TeamValue = cache_work_json:encode_json_team(SuperTeam),
			{<<"team">>,TeamValue};
          {"tower", Tower} ->
            TowerValue = cache_work_json:encode_json_tower(Tower),
            {<<"tower">>, TowerValue};
          {"tollgate", Tollgate} ->
            TollgateValue = cache_work_json:encode_json_tollgate(Tollgate),
            {<<"tollgate">>, TollgateValue};
          {"backpack", Backpack} ->
            BackpackValue = cache_work_json:encode_json_backpack(Backpack),
            {<<"backpack">>, BackpackValue};
          {"mission", Mission} ->
            MissionValue = cache_work_json:encode_json_mission(Mission#mission.mission_list, Mission#mission.player_activity),
            {<<"mission">>, MissionValue};
          {"achievement", Achievement} ->
            AchievementValue = cache_work_json:encode_json_achievement_list(Achievement#achievement.group_list),
            {<<"achievement">>, AchievementValue};
          {"shop", Shop} ->
            {<<"shop">>, cache_work_json:encode_json_shop(Shop)};
          {"shop_discount",_}->
            {<<"shop_discount">>,[]};
          {"guide_step", Step} ->
            {<<"guide_step">>, Step};
          {"step_guide", StepGuide} ->
            {<<"step_guide">>, cache_work_json:encode_json_guide_step(StepGuide)};
          {"gift_close", GiftClose} ->
            {<<"gift_close">>, GiftClose};
          {"rank_reward_show", ShowRankReward} ->
            {<<"rank_reward_show">>, ShowRankReward};
          {"strength_buy_times", BuyTimes} ->
            {<<"strength_buy_times">>, BuyTimes};
          {"lottery", LotteryList} ->
            {<<"lottery">>, cache_work_json:encode_json_lottery(LotteryList)}
        end
      end, UserValueList),
  Data = {struct, JsonList},
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

encode_ss_backpack(Backpack) ->
	BackpackValue = cache_work_json:encode_json_backpack(Backpack),
    JsonList = [{<<"backpack">>, BackpackValue}],
  	Data = {struct, JsonList},
  	dd_util:to_binary(dd_util:encode_json_utf8(Data)).

encode_ss_rewardmatch(Reward_match) ->
	Value = cache_work_json:encode_json_reward_match(Reward_match),
    JsonList = [{<<"reward_match">>, Value}],
  	Data = {struct, JsonList},
  	dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%格式化正常模式球员队伍
encode_super_team(SuperTeam)->
	TeamValue = cache_work_json:encode_json_change_team(SuperTeam),
	JsonList = [{<<"team">>,TeamValue}],
	Data = {struct, JsonList},
	dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%格式化无尽排名中获取其他玩家基本数据接口的相关返回数据
encode_user_basic_info(InfoList) ->
  Data = {struct, [{<<"user_info">>, cache_work_json:encode_json_basic_info(InfoList)}]},
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%关卡相关系统%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%关卡结算数据
encode_game_end(<<"activity_game_end">>, Account, {UpdatedMissionList, UpdatedAchievementGroupList, ActivityItem, TollgateRankList, DropList}) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"activity_tollgate">>, cache_work_json:encode_json_activity_tollgate(ActivityItem, TollgateRankList)},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdatedMissionList, Account#account.mission#mission.player_activity)},
        {<<"update_achievement">>, cache_work_json:encode_json_achievement_list(UpdatedAchievementGroupList)},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"drop_list">>, cache_work_json:encode_json_tollgate_drop(DropList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data));
encode_game_end(<<"endless_game_end">>, Account, {UpdatedMissionList, UpdatedAchievementGroupList, EndLessItem, DropList}) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"endless_tollgate">>, cache_work_json:encode_json_endless_tollgate(EndLessItem, [])},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdatedMissionList, Account#account.mission#mission.player_activity)},
        {<<"update_achievement">>, cache_work_json:encode_json_achievement_list(UpdatedAchievementGroupList)},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"drop_list">>, cache_work_json:encode_json_endless_drop(DropList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data));
encode_game_end(<<"game_end">>, Account, {UpdatedMissionList, UpdatedAchievementGroupList, UpdateTollgate, TollgateRankList, DropList}) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"update_tollgate">>, cache_work_json:encode_json_tollgate_mode(UpdateTollgate, TollgateRankList)},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdatedMissionList, Account#account.mission#mission.player_activity)},
        {<<"update_achievement">>, cache_work_json:encode_json_achievement_list(UpdatedAchievementGroupList)},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"drop_list">>, cache_work_json:encode_json_tollgate_drop(DropList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%普通关卡和活动关卡进入关卡前请求种子的返回数据格式化
encode_enter_tollgate(Account, {Seed, RandValue}) when is_record(Account, account) andalso is_integer(Seed) andalso is_integer(RandValue) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"germ">>, Seed},
        {<<"rand_val">>, RandValue}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%无尽关卡掉落种子获取接口返回数据格式化
encode_endless_germ(Account) when is_record(Account, account) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"endless_germ">>, Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.seed_val},
        {<<"endless_rand_val">>, Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.rand_val}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%世界地图的采集障碍物
encode_world_map_block(Account, UpdateWorldMapBlockItem, UpdateAchievementList) when is_record(Account, account)
  andalso is_record(UpdateWorldMapBlockItem, harvest_obstacles_item) andalso is_list(UpdateAchievementList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"world_map_block">>, cache_work_json:encode_json_map_block_item(UpdateWorldMapBlockItem)},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"update_achievement">>, cache_work_json:encode_json_achievement_list(UpdateAchievementList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%扫荡
encode_tollgate_sweep(Account, {UpdateMissionList, UpdateTollgate, DropList}) when is_record(Account, account) andalso is_list(UpdateMissionList) andalso is_record(UpdateTollgate, tollgate) andalso is_list(DropList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"update_tollgate">>, cache_work_json:encode_json_tollgate_mode(UpdateTollgate, [])},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdateMissionList, Account#account.mission#mission.player_activity)},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"drop_list">>, cache_work_json:encode_json_tollgate_sweep_drop(DropList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%进阶或者升级强化等相关系统%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%铭文合成返回数据格式化
encode_compose_inscription(Account) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%铭文进阶数据格式化
encode_advance_inscription(Account, Tower) when is_record(Account, account) andalso is_record(Tower, character) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"tower">>, cache_work_json:encode_json_tower_item(Tower)},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%装备升级进阶返回数据的格式化
encode_equipment_strengthen(Account, EquipmentItem, UpdateAchievementList, UpdateMissionList) when is_record(Account,account)
  andalso is_record(EquipmentItem,equipment) andalso is_list(UpdateMissionList) andalso is_list(UpdateAchievementList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"object">>, cache_work_json:encode_json_equipment_item(EquipmentItem)},
        {<<"tower">>, cache_work_json:encode_json_tower(Account#account.heros)},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdateMissionList, Account#account.mission#mission.player_activity)},
        {<<"update_achievement">>, cache_work_json:encode_json_achievement_list(UpdateAchievementList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%角色铭文替换返回数据格式化
encode_replace_inscription(Account, TowerItem) when is_record(Account, account) andalso is_record(TowerItem, character) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"tower">>, cache_work_json:encode_json_tower_item(TowerItem)},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%角色替换返回数据格式化
encode_update_tower_team(Account) when is_record(Account, account) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"tower">>, cache_work_json:encode_json_tower(Account#account.heros)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%商城相关%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%商城购买物品返回数据格式化
encode_buy_goods(Account, MissionList) when is_record(Account, account) andalso is_list(MissionList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"shop">>, cache_work_json:encode_json_shop(Account#account.shop)},
        {<<"update_mission">>, cache_work_json:encode_json_mission(MissionList, Account#account.mission#mission.player_activity)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%快速购买返回数据格式化
encode_fast_purchase(Account) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"strength_buy_times">>, Account#account.strength#strength.today_buy_times}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%激活码兑换的返回数据格式化
encode_exchange_cdkey(Account, CDKeyPackageList) when is_record(Account, account) andalso is_list(CDKeyPackageList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"package">>, cache_work_json:encode_json_cdkey_package(CDKeyPackageList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%奖励相关系统%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%获取成就奖励的返回数据格式化
encode_achievement_reward(Account, AchievementList) when is_record(Account, account) andalso is_list(AchievementList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"achievement">>, cache_work_json:encode_json_achievement_list(AchievementList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%获取活跃度奖励的返回数据的格式化
encode_activity_reward(Account) when is_record(Account, account) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"activity">>, cache_work_json:encode_json_activity(Account#account.mission#mission.player_activity)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%获取任务奖励的返回数据的格式化
encode_mission_reward(Account, MissionList) when is_record(Account, account) andalso is_list(MissionList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"mission_list">>, cache_work_json:encode_json_mission_list(MissionList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%获取登录奖励的返回数据的格式化
encode_login_reward(Account) when is_record(Account, account) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"login_reward">>, cache_work_json:encode_json_login_reward(Account#account.login_reward)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%排名相关系统%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%格式化好友无尽排名
encode_friend_endless_rank({NoScoreList, LastWeekList, ThisWeekList}) ->
  Data =
    {
      struct,
      [
        {<<"lastweek_rank">>, cache_work_json:encode_json_friend_endless_rank(LastWeekList)},
        {<<"thisweek_rank">>, cache_work_json:encode_json_friend_endless_rank(ThisWeekList)},
        {<<"norecord_rank">>, cache_work_json:encode_json_friend_endless_rank(NoScoreList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%格式化全服无尽排名
encode_server_endless_rank({Top20List, Self}) ->
  Data = cache_work_json:encode_json_server_endless_rank(Top20List, Self),
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%邮件相关数据%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%领取邮件附件接口的返回数据的格式化
encode_attach_mail(Account, UpdateMissionList, UpdateAchievementList) when is_record(Account, account) andalso is_list(UpdateMissionList) andalso is_list(UpdateAchievementList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdateMissionList, Account#account.mission#mission.player_activity)},
        {<<"update_achievement">>, cache_work_json:encode_json_achievement_list(UpdateAchievementList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%全部领取邮件附件的返回数据的格式化
encode_mass_attach_mail(Account, UpdateMissionList, UpdateAchievementList) when is_record(Account, account) andalso is_list(UpdateMissionList) andalso is_list(UpdateAchievementList) ->
  encode_attach_mail(Account, UpdateMissionList, UpdateAchievementList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%支付相关接口返回数据格式化%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%ios正版渠道支付相关数据格式化
encode_iap_buy(Account, UpdateMissionList) when is_record(Account, account) andalso is_list(UpdateMissionList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"shop">>, cache_work_json:encode_json_shop(Account#account.shop)},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdateMissionList, Account#account.mission#mission.player_activity)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

encode_platform_pay(Account, UpdateMissionList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"shop">>, cache_work_json:encode_json_shop(Account#account.shop)},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdateMissionList, Account#account.mission#mission.player_activity)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%寻宝系统返回数据格式化%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%抽奖操作返回数据格式化
encode_lottery(Account, LotteryList, LotteryConfigList, UpdateMissionList) when is_record(Account, account) andalso is_list(LotteryList) andalso is_list(LotteryConfigList) andalso is_list(UpdateMissionList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"tower">>, cache_work_json:encode_json_tower(Account#account.heros)},
        {<<"backpack">>, cache_work_json:encode_json_backpack(Account#account.backpack)},
        {<<"lottery_list">>, cache_work_json:encode_json_lottery_list(LotteryList)},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdateMissionList, Account#account.mission#mission.player_activity)},
        {<<"lottery">>, cache_work_json:encode_json_lottery(LotteryConfigList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%好友系统的相关操作的接口%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%赠送好友体力相关的接口返回数据格式化
encode_present_friend_strength(Account, UpdateMissionList, UpdateAchievementList) when is_record(Account, account) andalso is_list(UpdateMissionList) andalso is_list(UpdateAchievementList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdateMissionList, Account#account.mission#mission.player_activity)},
        {<<"update_achievement">>, cache_work_json:encode_json_achievement_list(UpdateAchievementList)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).

%%炫耀分数相关接口
encode_share_score(Account, UpdateMissionList) when is_record(Account, account) andalso is_list(UpdateMissionList) ->
  Data =
    {
      struct,
      [
        {<<"player">>, cache_work_json:encode_json_player(get_player_basic_data(Account))},
        {<<"update_mission">>, cache_work_json:encode_json_mission(UpdateMissionList, Account#account.mission#mission.player_activity)}
      ]
    },
  dd_util:to_binary(dd_util:encode_json_utf8(Data)).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%返回玩家基本数据
%%{ID, Name，gold, gem, energy, cd,rm_ticket,rm_ticket_cd}
get_player_basic_data(Account) ->
  {
    Account#account.platform_info#platform_info.player_id,
    Account#account.platform_info#platform_info.player_dis_name,
    Account#account.gold_coin,
    Account#account.gem,
    Account#account.strength#strength.strength,
    cache_update_to_time:get_strength_cd(Account#account.strength),
	Account#account.reward_match_ts#ts_item.count,
	cache_update_to_time:get_ts_item_cd(Account#account.reward_match_ts,?TICKET_RECOVER_TIME)
  }.