%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十月 2014 下午2:48
%%%-------------------------------------------------------------------
-module(cache_log_util).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
-include("cache_def.hrl").
-include("../mail/mail.hrl").
-include("../csv.hrl").
-include("../ranking/ranking.hrl").

%% API
-export([
  write_game_end_log/2,
  write_lottery_log/5,
  write_login_log/4,
  write_register_log/6,
  write_reward_log/7,
  write_success_pay_log/9,
  write_fail_pay_log/5,
  write_consume_log/8,
  write_strengthen_log/10,
  write_mission_log/5,
  write_achievement_log/5
]).

write_game_end_log(Uin, GameEndData) when is_integer(Uin) andalso is_record(GameEndData, game_end) ->
  DropStr = cache_api:lists_to_str_with_split(cache_api:flatten_statistics_list(GameEndData#game_end.gain_drops), ";"),
  PropStr = cache_api:lists_to_str_with_split(cache_api:flatten_statistics_list(GameEndData#game_end.use_props), ";"),
  TollgateID = GameEndData#game_end.tollgate_id,
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  GainGold =
    case GameEndData#game_end.success of
      0 -> TollgateConfig#res_stage.gain_gold;
      _ -> 0
    end,

  TollgateType =
    if
      TollgateID < 10000 -> 1;
      TollgateID < 20000 -> 2;
      TollgateID < 30000 -> 3;
      true -> throw({custom, "HintTollgateDataError"})
    end,
  cache_log:log_game_end_flow(Uin, dd_util:timestamp(), GameEndData#game_end.success, TollgateID, TollgateType, GainGold,
    GameEndData#game_end.gain_star, GameEndData#game_end.gain_score, GameEndData#game_end.endless_tollgate_num, PropStr, DropStr).

write_lottery_log(Uin, LotteryType, CostType, CostCount, DropList) when is_integer(Uin) andalso is_integer(LotteryType) andalso is_integer(CostType) andalso is_integer(CostCount) andalso is_list(DropList) ->
  Drop = lists:map(fun(Item) -> {Item#treasure_item.drop_id, Item#treasure_item.count} end, DropList),
  DropStr = cache_api:lists_to_str_with_split(cache_api:flatten_statistics_list(Drop), ";"),
  cache_log:log_lottery_flow(Uin, LotteryType, CostType, CostCount, DropStr).

%%plattype: 0 匿名，1 facebook, -1:未知
write_login_log(Uin, PlatType, Device, Ip) when is_integer(Uin) andalso is_integer(PlatType) andalso is_list(Device) andalso is_list(Ip) ->
  cache_log:log_login(Uin, PlatType, dd_util:timestamp(), Device, Ip).

write_register_log(Uin, PlatType, PlatID, PlatDisName, Device, Ip) when is_integer(Uin) andalso is_integer(PlatType) andalso is_list(PlatID) andalso is_list(PlatDisName) andalso is_list(Device) andalso is_list(Ip)->
  cache_log:log_register(Uin, dd_util:timestamp(), PlatType, PlatID, PlatDisName, Device, Ip).

%%log_reward_flow(Uin, RewardType, GetRewardTime, RewardItemType, RewardItemID, RewardItemCount) ->
%%获取奖励的类型
get_reward_type(RewardType) when is_list(RewardType) ->
  case RewardType  of
    "LoginReward" -> 1;
    "Activity" -> 2;
    "Mission" -> 3;
    "Achievement" -> 4;
    _ -> -1
  end;
get_reward_type(RewardType) -> RewardType.

%%RewardItemType=>  1: 金币， 2=> 钻石  3=> 活跃度  4=> 道具  5=> 体力
write_reward_log(Uin, RewardType, GetRewardTime, RewardItemType, RewardItemID, RewardItemCount, RewardDesc) when is_integer(Uin) andalso is_list(RewardType) andalso is_integer(GetRewardTime) andalso is_integer(RewardItemType) andalso is_list(RewardItemID) andalso is_integer(RewardItemCount) andalso is_list(RewardDesc) ->
  cache_log:log_reward_flow(Uin, get_reward_type(RewardType), GetRewardTime, RewardItemType, RewardItemID, RewardItemCount, RewardDesc).

%%BuyItemType: 1 => 钻石 -1=> 未知 : BuyDesc: 存储orders， BuyBackup: 存储失败的原因
write_success_pay_log(Uin, Plat, PayOrder, PayItemID, BuyItemType, BuyItemID, BuyCount, BuyDesc, BuyBackup) when is_integer(Uin) andalso is_list(PayItemID) andalso is_integer(BuyItemType) andalso is_list(BuyItemID) andalso is_integer(BuyCount) andalso is_list(BuyDesc) andalso is_list(BuyBackup) ->
  cache_log:log_success_pay_flow(Uin, Plat, PayOrder, dd_util:timestamp(), PayItemID, BuyItemType, BuyItemID, BuyCount, BuyDesc, BuyBackup).
write_fail_pay_log(Uin, Plat, PayOrder, PayInfo, Reason) when is_integer(Uin) andalso is_list(Reason) ->
  cache_log:log_fail_pay_flow(Uin, Plat, PayOrder, dd_util:timestamp(), PayInfo, Reason).

%%BuyItemType: 1 => 钻石 2: 金币，3：道具礼包，4: 道具  5： 快捷购买体力， 6：快捷购买金币， 7：背包扩容  -1=> 未知
%%CostType: 1 钻石， 2 金币
write_consume_log(Uin, BuyItemType, CommodityID, BuyItemID, BuyItemCount, CostType, CostCount, BuyDesc) when is_integer(Uin) andalso is_integer(BuyItemType) andalso is_list(CommodityID) andalso is_list(BuyItemID) andalso is_integer(BuyItemCount) andalso is_integer(CostType) andalso is_integer(CostCount) andalso is_list(BuyDesc) ->
  cache_log:log_consume_flow(Uin, dd_util:timestamp(), BuyItemType, CommodityID, BuyItemID, BuyItemCount, CostType, CostCount, BuyDesc).

%%strengthenType: 0 升级， 1 进阶
%%strengthenObjID:  进阶目标的id
write_strengthen_log(Uin, StrengthenType, StrengthenObjID, StrengthenObjNo, ConsumeGoldCount, SwallowList, BeforeObjNo, BeforeObjExp, AfterObjNo, AfterObjExp) when is_integer(Uin) andalso is_integer(StrengthenType) andalso is_list(StrengthenObjID) andalso is_list(StrengthenObjNo) andalso is_integer(ConsumeGoldCount) andalso is_list(SwallowList) andalso is_list(BeforeObjNo) andalso is_integer(BeforeObjExp) andalso is_list(AfterObjNo) andalso is_integer(AfterObjExp) ->
  SwallowStr = cache_api:lists_to_str_with_split(cache_api:flatten_statistics_list(SwallowList), ";"),
  cache_log:log_strengthen_flow(Uin, StrengthenType, StrengthenObjID, StrengthenObjNo, ConsumeGoldCount, SwallowStr, BeforeObjNo, BeforeObjExp, AfterObjNo, AfterObjExp).

%%mission:
%%missionType: 1: 期限任务   2：每日任务  3. 活跃度任务
%%MissionStatus: 1: 未完成   2. 已完成
write_mission_log(Uin, MissionType, MissionID, MissionDesc, MissionStatus) when
  is_integer(Uin) andalso is_integer(MissionType) andalso is_integer(MissionStatus) andalso is_list(MissionID) andalso is_list(MissionDesc) ->
  cache_log:log_mission_flow(Uin, MissionType, MissionID, MissionDesc, MissionStatus).

write_achievement_log(Uin, AchievementType, AchievementID, AchievementDesc, AchievementStatus) when is_integer(Uin) andalso is_integer(AchievementType) andalso is_integer(AchievementStatus) andalso is_list(AchievementID) andalso is_list(AchievementDesc)->
  cache_log:log_achievement_flow(Uin, AchievementType, AchievementID, AchievementDesc, AchievementStatus).

