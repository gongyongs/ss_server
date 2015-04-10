%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 八月 2014 上午10:26
%%%-------------------------------------------------------------------
-module(cache_update_to_time).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
-include("cache_def.hrl").
-include("cache.hrl").
-include("../csv.hrl").
%% API
-export([
  update_strength/1,
  update_ts_info/4,
  update_harvest_obstacle/1,
  update_login/1,
  update_daily_shop/1,
  update_daily_mission/1,
  update_daily_achievement/1,
  update_daily_tollgate/1,
  update_daily_strength/1,
  update_daily_lottery/1
]).

-export([
  get_strength_cd/1,
  get_ts_item_cd/2
]).



-define(STRENGTH_COOL_DOWN_TIME, (5*60)).
-define(HARVEST_RECOVER_TIME, 24 * 60 * 60).
-define(TS_RM_INIT, 5).
-define(TS_RM_CDT, 8*60*60).

get_strength_cd(Strength) when is_record(Strength, strength) ->
  CurTime = dd_util:timestamp(),
  Val = Strength#strength.strength,
  LastTs = Strength#strength.last_update_ts,
  if
    Val >= 100 -> 0;
    LastTs =:= 0 -> ?STRENGTH_COOL_DOWN_TIME;
    true -> (?STRENGTH_COOL_DOWN_TIME - ((CurTime - Strength#strength.last_update_ts) rem ?STRENGTH_COOL_DOWN_TIME))
  end.

get_ts_item_cd(Ts_item,CoolDown) when is_record(Ts_item, ts_item) ->
  CurTime = dd_util:timestamp(),
  Val = Ts_item#ts_item.count,
  LastTs = Ts_item#ts_item.last_update_ts,
  if
    Val >= 100 -> 0;
    LastTs =:= 0 -> CoolDown;
    true -> (CoolDown - ((CurTime - LastTs) rem CoolDown))
  end.

%%更新体力值
%%{strength, is_update}
update_strength(Strength) when is_record(Strength, strength) ->
  CurTime = dd_util:timestamp(),
  LastRecoverTime = Strength#strength.last_update_ts,
  InitCountConfig = cache_csv:get_init_account_cfg(1),
  InitStrengthValue = InitCountConfig#res_init_account.init_strength,
  CurStrengthValue = Strength#strength.strength,

  TimeDif = CurTime - LastRecoverTime,

  Value = TimeDif div ?STRENGTH_COOL_DOWN_TIME,

  case Value of
      0 ->
        {"strength", Strength, false};
      _ ->
          if
            CurStrengthValue >= InitStrengthValue ->
              {"strength", Strength#strength{last_update_ts = CurTime}, true};
            true ->
              StrengthValue = CurStrengthValue + Value,
              SValue =
                if
                  StrengthValue >=InitStrengthValue -> InitStrengthValue;
                  true -> StrengthValue
                end,
              {"strength", Strength#strength{strength = SValue, last_update_ts = CurTime}, true}
          end
  end.

%%更新时间回复单位
update_ts_info(Ts_info,InitValue,CDTime,UpdateName) when is_record(Ts_info, ts_item) ->
  CurTime = dd_util:timestamp(),
  LastRecoverTime = Ts_info#ts_item.last_update_ts,
  CurCount = Ts_info#ts_item.count,
  TimeDif = CurTime - LastRecoverTime,
  Value = TimeDif div CDTime,

  case Value of
      0 ->
        {UpdateName, Ts_info, false};
      _ ->
          if
            CurCount >= InitValue ->
              {UpdateName, Ts_info#ts_item{last_update_ts = CurTime}, true};
            true ->
              NewValue = CurCount + Value,
              SValue =
                if
                  NewValue >=InitValue -> InitValue;
                  true -> NewValue
                end,
              {UpdateName, Ts_info#ts_item{ count= SValue, last_update_ts = CurTime}, true}
          end
  end.

update_harvest_obstacle(Tollgates) when is_record(Tollgates, stage)->
  {NewHarvestList, Num} = check_harvest_obstacle(Tollgates#stage.harvest_obstacles_list),
  ?FILE_LOG_DEBUG("update_harvest_obstacle : num = ~p", [Num]),
  if
    Num > 0 ->
      {"stage", Tollgates#stage{harvest_obstacles_list = NewHarvestList}, true};
    true ->
      {"stage", Tollgates, false}
  end.

update_daily_tollgate(Tollgate) when is_record(Tollgate, stage) ->
  NTollgateList = check_tollgate_mode(Tollgate#stage.base_tollgate_list),
  NActivityList = check_activity_tollgate_mode(Tollgate#stage.ac_tollgate),
  NEndDrop = Tollgate#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop{end_ts = 0, endless_drop = [], seed_ts = 0, seed_val = 0},
  TollgateDrop = Tollgate#stage.tollgate_drop#tollgate_drop{end_ts = 0, seed_ts = 0, seed_val = 0},
  EndlessItem = Tollgate#stage.endless_tollgate#endless_tollgate{week_score_record = [], endless_drop = NEndDrop},
  Tollgate#stage{ac_tollgate = NActivityList, tollgate_drop = TollgateDrop, base_tollgate_list = NTollgateList, endless_tollgate = EndlessItem}.

update_daily_strength(Strength) when is_record(Strength, strength) ->
  Strength#strength{give_friend_strength = [], today_buy_times = 0}.

%%返回是否是本天第一次登陆
update_login(LoginReward) when is_record(LoginReward, login_reward)  ->
  CurTime = dd_util:timestamp(),
  LastLoginTime = LoginReward#login_reward.latest_login_ts,
  LastLoginDate = dd_util:to_local_time(dd_util:timestamp_to_datetime(LastLoginTime)),
  {C, T} = dd_util:to_local_time(dd_util:timestamp_to_datetime(CurTime)),

  %%判断上一次登陆是否在刷新时间之前
  case dd_util:time_compare_by_datetime(LastLoginDate, {C, ?LOGIN_REWARD_FRESH_TIME}) of
    -1 ->           %%上一次登陆时间是在今天刷新时间之前
      case dd_util:time_compare_by_datetime({C,T}, {C, ?LOGIN_REWARD_FRESH_TIME}) of
        -1 ->       %%本次登陆是在刷新时间之前
          {LoginReward#login_reward{latest_login_ts = CurTime}, false};
        _ ->    %%本次登陆在刷新时间之后
          %%需要判断是否是连续登陆以确定登陆奖励以及连续登陆天数
          {{Y,M,D},_} = dd_util:to_local_time(dd_util:timestamp_to_datetime(CurTime)),
          {Y1,M1,D1} = dd_util:get_last_day({Y,M,D}),
          TotalLoginDay = LoginReward#login_reward.total_login_days + 1,
          case dd_util:time_compare_by_datetime(LastLoginDate, {{Y1,M1,D1}, ?LOGIN_REWARD_FRESH_TIME}) of
            -1 -> %%上一次登陆时在上一次刷新之前，所以清除连续登陆的数据
              {LoginReward#login_reward{login_times = 1, total_login_days =  TotalLoginDay, latest_login_ts = CurTime, login_reward_list = [#login_reward_item{id = 1, login_ts = CurTime}]}, true};
            _ -> %%上一次登陆时在上一次刷新之后，添加连续登陆奖励
              LoginTimes = LoginReward#login_reward.login_times + 1,
              NLoginTimes =
                if
                  LoginTimes > 7 -> 7;
                  true -> LoginTimes
                end,
              List = [#login_reward_item{id = NLoginTimes, login_ts = CurTime} | LoginReward#login_reward.login_reward_list],
              {LoginReward#login_reward{login_times = LoginTimes, login_reward_list = List, latest_login_ts = CurTime, total_login_days = TotalLoginDay},true}
          end
      end;
    _ ->       %%上一次登陆时在今天刷新时间之后
      {LoginReward#login_reward{latest_login_ts = CurTime}, false}
  end.

update_daily_shop(Shop) when is_record(Shop, shop)  ->
  cache_account_def:load_shop_from_config(Shop).

update_daily_mission(Mission) when is_record(Mission, mission) ->
  cache_mission:reset_mission_daily(Mission).

update_daily_achievement(Achievement) when is_record(Achievement, achievement) ->
  cache_mission:reset_achievement_daily(Achievement).

update_daily_lottery(Lottery) when is_record(Lottery, lottery) ->
  Lottery#lottery{last_lottery_ts = 0, today_lottery_lists = []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_tollgate_mode(TollgateList) ->
  check_tollgate_mode_1(TollgateList, []).
check_tollgate_mode_1([], OutList) -> lists:reverse(OutList);
check_tollgate_mode_1([TollgateItem | T], OutList) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateItem#tollgate.id),
  check_tollgate_mode_1(T, [TollgateItem#tollgate{daily_remain_times = TollgateConfig#res_stage.daily_restrict_count, cool_time = TollgateConfig#res_stage.cool_time, addition_remain_times = TollgateConfig#res_stage.daily_addition_count} | OutList]).

check_activity_tollgate_mode(TollgateList) ->
  check_activity_tollgate_mode_1(TollgateList, []).
check_activity_tollgate_mode_1([], OutList) -> lists:reverse(OutList);
check_activity_tollgate_mode_1([TollgateItem | T], OutList) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateItem#activity_tollgate_item.tollgate_id),
  check_activity_tollgate_mode_1(T, [TollgateItem#activity_tollgate_item{daily_remain_times = TollgateConfig#res_stage.daily_restrict_count, cool_time = TollgateConfig#res_stage.cool_time} | OutList]).


check_harvest_obstacle(HarvestList) ->
  check_harvest_obstacle_1(HarvestList, [], 0).
check_harvest_obstacle_1([], Out, Value) -> {lists:reverse(Out), Value};
check_harvest_obstacle_1([HarvestItem | T], Out, Value) ->
  CurTime = dd_util:timestamp(),
  RemainTimes = HarvestItem#harvest_obstacles_item.remain_number,
  TimeDif = CurTime - HarvestItem#harvest_obstacles_item.last_update_ts,
  %%?FILE_LOG_DEBUG("check havest, item = ~p, timedif = ~p, DIF = ~p, curtime = ~p", [HarvestItem, TimeDif, ?HARVEST_RECOVER_TIME, CurTime]),
  if
    RemainTimes =< 0 andalso TimeDif >= ?HARVEST_RECOVER_TIME ->
      check_harvest_obstacle_1(T, [HarvestItem#harvest_obstacles_item{remain_number = 3, last_update_ts = CurTime} | Out], Value + 1);
    true ->
      check_harvest_obstacle_1(T, [HarvestItem | Out], Value)
  end.

