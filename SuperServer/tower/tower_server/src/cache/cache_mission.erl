%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 八月 2014 下午6:15
%%%-------------------------------------------------------------------
-module(cache_mission).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
-include("cache_def.hrl").
-include("../csv.hrl").

%% API
-export([
  init_mission/1,
  load_all_mission_from_config/1,
  update_mission/4,
  reset_mission_daily/1
]).

-export([
  init_achievement/1,
  update_achievement/4,
  reset_achievement_daily/1,
  sync_config/1,
  update_achievement_daily/1
]).

-export([
  update_mission_data/4,
  update_mission_item_with_consumption_data/2,
  update_mission_item_with_game_end_data/2,
  update_mission_item_with_strengthen_data/2,
  update_mission_item_with_share/2,
  update_mission_item_with_strength_gift/2,
  update_mission_item_with_lottery_data/2,
  update_mission_item_with_charge_data/2
]).

-export([
  update_achievement_list/4,
  update_achievement_item_with_rank_data/2,
  update_achievement_item_with_game_end_data/2,
  update_achievement_item_with_strengthen_data/2,
  update_achievement_item_with_strength_gift/2,
  update_achievement_item_with_friend_data/2,
  update_achievement_item_with_harvest_obstacle/2
]).

%%sync_config  同步配置文件，用于内部测试，配置文件改动较大时
sync_config(Account) when is_record(Account, account) ->
 %% NGroupList = clear_non_existent_achievement(Account#account.achievement#achievement.group_list),
  Achievement = Account#account.achievement#achievement{group_list = []},
  NAchievement = load_achievement_from_config(Achievement),
  MissionList = clear_non_existent_mission(Account#account.mission#mission.mission_list),
  Mission =  Account#account.mission#mission{mission_list = MissionList},
  NMission = load_all_mission_from_config(Mission),
  {Account#account{achievement = NAchievement, mission = NMission}, ["achievement", "mission"]}.


%%返回最新mission
init_mission(InitMission) ->
   Mission = InitMission#mission{
    mission_list = [],
    player_activity = #player_activity{activity_value = 0, activity_reward_list = []}
  },
  load_all_mission_from_config(Mission).

%%创建新玩家是调用，还需要一个函数作为更新的接口
load_all_mission_from_config(Mission) when is_record(Mission, mission) ->
  TaskList = cache_csv:get_all_task_config(),
  lists:foldl(
    fun(TaskElement, TmpMission) ->
      MissionItem = #mission_item{mission_id = TaskElement#res_task.id, mission_type = TaskElement#res_task.task_type, mission_progress_1 = 0, mission_progress_2 = 0, mission_finish_ts = 0, mission_get_reward_ts = 0,mission_record = [], mission_finish_times = 0},
      CurTime = dd_util:timestamp(),
      case TaskElement#res_task.task_type of
        1 ->  %%期限任务
          if
            CurTime > TaskElement#res_task.start_ts orelse CurTime =< TaskElement#res_task.over_ts -> %%期限任务未开始或者期限任务已结束
              TmpMission;
            true ->  %%已开始且未结束
              case TaskElement#res_task.daily_refresh of
                1 -> %%累计任务
                  MissionList =  cache_util:insert_mission_by_id(TmpMission#mission.mission_list, MissionItem, false),  %%累计任务不处理
                  TmpMission#mission{mission_list = MissionList};
                2 -> %%每日任务
                  MissionList =  cache_util:insert_mission_by_id(TmpMission#mission.mission_list, MissionItem, true),  %%每日任务更新处理
                  TmpMission#mission{mission_list = MissionList}
              end
          end;
        2 ->  %%每日任务
          MissionList =  cache_util:insert_mission_by_id(TmpMission#mission.mission_list, MissionItem, true),  %%每日任务更新处理
          TmpMission#mission{mission_list = MissionList};
        3 ->  %%活跃度任务: 属于每日任务，更新处理
          MissionList =  cache_util:insert_mission_by_id(TmpMission#mission.mission_list, MissionItem, true),  %%活跃度任务每日刷新更新处理
          TmpMission#mission{mission_list = MissionList};
        _ ->
          ?FILE_LOG_WARNING("task config element type error", [])
      end
    end, Mission, TaskList).

reset_mission_daily(Mission) when is_record(Mission, mission) ->
  %%期限任务或者每周任务 清除到期的任务
  MissionList = clear_overdue_task(Mission#mission.mission_list),
  NMission = Mission#mission{mission_list = MissionList, player_activity = #player_activity{activity_value = 0, activity_reward_list = []}},
  load_all_mission_from_config(NMission).

clear_overdue_task(List) when is_list(List) ->
  clear_overdue_task_1(List, []).
clear_overdue_task_1([], OutList) -> OutList;
clear_overdue_task_1([MissionItem | T], OutList) ->
  TaskConfig = cache_csv:get_mission_config(MissionItem#mission_item.mission_id),
  CurTime = dd_util:timestamp(),
  StartTs = TaskConfig#res_task.start_ts,
  OverTs = TaskConfig#res_task.over_ts,
  if
    StartTs =/= 0 ->
      if
        CurTime >= StartTs andalso CurTime < OverTs ->
          clear_overdue_task_1(T, [MissionItem | OutList]);
        true -> clear_overdue_task_1(T, OutList)
      end;
    true ->
      clear_overdue_task_1(T, [MissionItem | OutList])
  end.

clear_non_existent_mission(MissionList) when is_list(MissionList) ->
  clear_non_existent_mission_1(MissionList, []).
clear_non_existent_mission_1([], OutList) -> OutList;
clear_non_existent_mission_1([MissionItem | T], OutList) ->
  case cache_csv:get_mission_config_without_exception(MissionItem#mission_item.mission_id) of
    {fail, _} -> clear_non_existent_mission_1(T, OutList);
    {success, _} -> clear_non_existent_mission_1(T, [MissionItem | OutList])
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_mission(game_end, Uin, Mission, GameEndData) when is_record(Mission, mission) andalso is_record(GameEndData, game_end) ->
  update_mission_data(Uin, Mission, fun update_mission_item_with_game_end_data/2, GameEndData);
update_mission(strengthen, Uin, Mission, StrengthenData) when is_record(Mission, mission) andalso is_record(StrengthenData, strengthen_data) ->
  update_mission_data(Uin, Mission, fun update_mission_item_with_strengthen_data/2, StrengthenData);
update_mission(consumption, Uin, Mission, {ConsumeType, BuyType, Count}) when is_record(Mission, mission) andalso is_integer(ConsumeType) andalso is_integer(BuyType) andalso is_integer(Count) ->
  update_mission_data(Uin, Mission, fun update_mission_item_with_consumption_data/2, {ConsumeType, BuyType, Count});
update_mission(charge, Uin, Mission, BuyGemCount) when is_record(Mission, mission) andalso is_integer(BuyGemCount) ->
  update_mission_data(Uin, Mission, fun update_mission_item_with_charge_data/2, BuyGemCount);
update_mission(lottery, Uin, Mission, LotteryID) when is_record(Mission, mission) andalso is_integer(LotteryID) ->
  update_mission_data(Uin, Mission, fun update_mission_item_with_lottery_data/2, LotteryID);
update_mission(strength_gift, Uin, Mission, {Type, Count}) when is_integer(Type) andalso is_integer(Count) andalso Count > 0 ->
  update_mission_data(Uin, Mission, fun update_mission_item_with_strength_gift/2, {Type, Count});
update_mission(share, Uin, Mission, ShareTs) ->
  update_mission_data(Uin, Mission, fun update_mission_item_with_share/2, ShareTs);
update_mission(sweep, Uin, Mission, {TollgateID, SweepTimes}) when is_record(Mission, mission) andalso is_integer(TollgateID) andalso  is_integer(SweepTimes) ->
  update_mission_data(Uin, Mission, fun update_mission_item_with_sweep/2, {TollgateID, SweepTimes}).

update_mission_data(Uin, Mission, Fun, Param) when is_function(Fun) ->
  %%更新所有任务
  {MissionList, UpdateList} = update_mission_list(Mission#mission.mission_list, Uin, Fun, Param),
  %%更新所有任务的数据
  {NUpdateList, PlayerActivity, NMissionList} = update_player_activity(UpdateList, Mission#mission.player_activity, MissionList),
  {NUpdateList, Mission#mission{player_activity = PlayerActivity, mission_list = NMissionList}}.

%%更新活跃度任务
update_player_activity(UpdateList, PlayerActivity, MissionList) when is_record(PlayerActivity, player_activity) andalso is_list(MissionList) andalso is_list(UpdateList) ->
  update_player_activity_1(UpdateList, PlayerActivity, MissionList, []).
update_player_activity_1([], NPlayerActivity, NMissionList, NUpdateList) -> {NUpdateList, NPlayerActivity, NMissionList};
update_player_activity_1([MissionItem | T], PlayerActivity, MissionList, UpdateList) ->
  TaskConfig = cache_csv:get_mission_config(MissionItem#mission_item.mission_id),
  case TaskConfig#res_task.task_type of
    3 ->  %%活跃度任务
     if
       MissionItem#mission_item.mission_finish_times >=  TaskConfig#res_task.daily_limit ->
         update_player_activity_1(T, PlayerActivity, MissionList, [MissionItem | UpdateList]);
       true ->
         case MissionItem#mission_item.mission_finish_ts of
           0 ->  update_player_activity_1(T, PlayerActivity, MissionList, [MissionItem | UpdateList]);
           _ ->
             FinishTimes = MissionItem#mission_item.mission_finish_times + 1,
             Progress1 = MissionItem#mission_item.mission_progress_1 - TaskConfig#res_task.condition_1,    %%清除一部分，剩余的继续累积
             Progress2 = TaskConfig#res_task.condition_2,        %%第二个参数目前当做条件用，未做累加
             NMissionItem =
               if
                 FinishTimes >= TaskConfig#res_task.daily_limit ->
                   MissionItem#mission_item{mission_progress_1 = Progress1, mission_finish_ts = dd_util:timestamp(), mission_finish_times = TaskConfig#res_task.daily_limit};
                 true ->
                   MissionItem#mission_item{mission_progress_1 = Progress1, mission_progress_2 = Progress2, mission_finish_ts = 0, mission_finish_times = FinishTimes}
               end,
             NPlayerActivity =
                case TaskConfig#res_task.reward_type of
                  3 ->
                    update_player_activity_value(PlayerActivity, TaskConfig#res_task.reward_num);
                  Other ->
                    ?FILE_LOG_ERROR("player activity reward type error, task.csv error [~p]", [Other]),
                    PlayerActivity
                end,

             NMissionList = cache_util:update_mission(MissionList, NMissionItem),
             update_player_activity_1(T, NPlayerActivity, NMissionList, [NMissionItem | UpdateList])
         end
     end;
    _ -> %%其他任务不处理
      update_player_activity_1(T, PlayerActivity, MissionList, [MissionItem | UpdateList])
  end.

%%更新活跃值以及奖励列表
update_player_activity_value(PlayerActivity, AddValue) when is_record(PlayerActivity, player_activity) andalso is_integer(AddValue) ->
  Activity = PlayerActivity#player_activity.activity_value,
  NActivity = Activity + AddValue,
  Reward = Activity div 20,
  NReward = NActivity div 20,
  RewardList =
    if
      NReward > Reward andalso Reward < 5 ->
        lists:foldr( fun(Idx, TmpFinishList) -> [#activity_reward_item{id = Idx*20, finish_ts = dd_util:timestamp(), get_reward_ts = 0} | TmpFinishList] end, [], lists:seq(Reward + 1, NReward));
      true -> []
    end,
  NList = lists:merge(RewardList, PlayerActivity#player_activity.activity_reward_list),
  PlayerActivity#player_activity{activity_reward_list = NList, activity_value = NActivity}.


update_mission_list(MissionList, Uin, Fun, Param) ->
  update_mission_list_1(MissionList, Uin, Fun, Param, [], []).
update_mission_list_1([], _, _, _, OutList, UpdateList) ->  {OutList, UpdateList};
update_mission_list_1([MissionItem | T], Uin, Fun, Param, OutList, UpdateList) ->
  {IsUpdate, NItem} = Fun(MissionItem, Param),
  case IsUpdate of
    true ->
      mission_log_flow(Uin, MissionItem, NItem),
      update_mission_list_1(T, Uin, Fun, Param, [NItem | OutList], [NItem | UpdateList]);
    false ->
      update_mission_list_1(T, Uin, Fun, Param, [NItem | OutList], UpdateList)
  end.

update_mission_item_with_sweep(MissionItem, {TollgateID, SweepTimes}) ->
  TaskConfig = cache_csv:get_mission_config(MissionItem#mission_item.mission_id),
  TaskType = TaskConfig#res_task.type_param_1,
  if
    MissionItem#mission_item.mission_finish_ts > 0 -> {false, MissionItem};
    TaskType < 30000 andalso TaskType > 20000 ->
      update_sweep_mission(MissionItem, TaskConfig, {TollgateID, SweepTimes});
    true ->
      {false, MissionItem}
  end.

update_mission_item_with_share(MissionItem, ShareTs) when is_integer(ShareTs) ->
  TaskConfig = cache_csv:get_mission_config(MissionItem#mission_item.mission_id),
  TaskType = TaskConfig#res_task.type_param_1,
  if
    MissionItem#mission_item.mission_finish_ts > 0 -> {false, MissionItem};
    TaskType < 80000 andalso TaskType > 70000 ->
      update_share_mission(MissionItem, TaskConfig, ShareTs);
    true ->
      {false, MissionItem}
  end.

update_mission_item_with_charge_data(MissionItem, GemCount) when is_record(MissionItem, mission_item) andalso is_integer(GemCount) andalso GemCount > 0->
  TaskConfig = cache_csv:get_mission_config(MissionItem#mission_item.mission_id),
  TaskType = TaskConfig#res_task.type_param_1,
  if
    MissionItem#mission_item.mission_finish_ts > 0 -> {false, MissionItem};
    TaskType < 70000 andalso TaskType > 60000 ->
      update_charge_mission(MissionItem, TaskConfig, GemCount);
    true ->
      {false, MissionItem}
  end.

update_mission_item_with_lottery_data(MissionItem, LotteryID) when is_record(MissionItem, mission_item) andalso is_integer(LotteryID) ->
  TaskConfig = cache_csv:get_mission_config(MissionItem#mission_item.mission_id),
  TaskType = TaskConfig#res_task.type_param_1,
  if
    MissionItem#mission_item.mission_finish_ts > 0 -> {false, MissionItem};
    TaskType < 70000  andalso TaskType > 60000 ->     %%抽奖任务
      update_lottery_mission(MissionItem, TaskConfig, LotteryID);
    true ->
      {false, MissionItem}
  end.

update_mission_item_with_strength_gift(MissionItem, {Type, Count}) when is_record(MissionItem, mission_item) andalso is_integer(Type) andalso is_integer(Count) andalso Count > 0 ->
  TaskConfig = cache_csv:get_mission_config(MissionItem#mission_item.mission_id),
  TaskType = TaskConfig#res_task.type_param_1,
  if
    MissionItem#mission_item.mission_finish_ts > 0 -> {false, MissionItem};
    TaskType > 70000 andalso TaskType < 80000 ->
      update_strength_gift_mission(MissionItem, TaskConfig, {Type, Count});
    true ->
      {false, MissionItem}
  end.

update_mission_item_with_consumption_data(MissionItem, {ConsumeType, BuyType, Count}) ->
  TaskConfig = cache_csv:get_mission_config(MissionItem#mission_item.mission_id),
  TaskType = TaskConfig#res_task.type_param_1,
    if
      MissionItem#mission_item.mission_finish_ts > 0 -> {false, MissionItem};
      TaskType < 40000  andalso TaskType > 30000 ->     %%消耗任务及购买任务
        update_consumption_mission(MissionItem, TaskConfig, {ConsumeType, BuyType, Count});
      true ->
        {false, MissionItem}
    end.

update_mission_item_with_game_end_data(MissionItem, GameEndData) when is_record(MissionItem, mission_item) andalso is_record(GameEndData, game_end) ->
  TaskConfig = cache_csv:get_mission_config(MissionItem#mission_item.mission_id),
  TaskType = TaskConfig#res_task.type_param_1,
    if
      MissionItem#mission_item.mission_finish_ts > 0 -> {false, MissionItem};
      TaskType < 20000  andalso TaskType > 10000 ->     %%击杀任务
        update_kill_monster_mission(MissionItem, TaskConfig, GameEndData#game_end.data_statistics);
      TaskType < 30000 andalso TaskType > 20000 andalso GameEndData#game_end.success >= 0 ->      %%通关任务
        update_tollgate_mission(MissionItem, TaskConfig, GameEndData#game_end.tollgate_id);
      TaskType < 50000 andalso TaskType > 40000 andalso GameEndData#game_end.success >= 0->      %%得分任务
        update_tollgate_score_mission(MissionItem,TaskConfig, GameEndData);
      TaskType > 30000 andalso TaskType < 40000 ->    %%消耗果冻任务
        update_consumption_mission(MissionItem, TaskConfig, {3, 0, GameEndData#game_end.jelly_consume_num});
      true ->
        {false, MissionItem}
    end.

update_mission_item_with_strengthen_data(MissionItem, StrengthenData) when is_record(MissionItem, mission_item) andalso  is_record(StrengthenData, strengthen_data) ->
  TaskConfig = cache_csv:get_mission_config(MissionItem#mission_item.mission_id),
  TaskType = TaskConfig#res_task.type_param_1,
    if
      MissionItem#mission_item.mission_finish_ts > 0 -> {false, MissionItem};
      TaskType > 50000 andalso TaskType < 60000 -> %%装备任务
        update_equipment_mission(MissionItem, TaskConfig, StrengthenData);
      true ->
        {false, MissionItem}
    end.

update_share_mission(MissionItem, TaskConfig, _ShareTs) ->
  case TaskConfig#res_task.type_param_1 of
    70001 ->
      Progress = MissionItem#mission_item.mission_progress_1 + 1,
      {true, update_mission_finish_ts(TaskConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress}, TaskConfig)};
    _ -> {false, MissionItem}
  end.

update_charge_mission(MissionItem, TaskConfig, GemCount) ->
  case TaskConfig#res_task.type_param_1 of
    60001 ->
      Progress = MissionItem#mission_item.mission_progress_1 + GemCount,
      {true, update_mission_finish_ts(TaskConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress}, TaskConfig)};
    _ -> {false, MissionItem}
  end.

update_lottery_mission(MissionItem, TaskConfig, _LotteryID) ->
  case TaskConfig#res_task.type_param_1 of
    60002 ->
      Progress = MissionItem#mission_item.mission_progress_1 + 1,
      {true, update_mission_finish_ts(TaskConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress}, TaskConfig)};
    _ -> {false, MissionItem}
  end.

update_strength_gift_mission(MissionItem, TaskConfig, {Type, _Count}) ->
  case TaskConfig#res_task.type_param_1 of
    70002 ->
      if
        Type =/= 1 -> {false, MissionItem};
        true ->
          Progress = MissionItem#mission_item.mission_progress_1 + 1,
          {true, update_mission_finish_ts(TaskConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress}, TaskConfig)}
      end;
    _ -> {false, MissionItem}
  end.

%%{false, Item}  %%BuyType: 2金币，1钻石，3 道具礼包 4道具  consumetype: 1 钻石，2 金币，3 果冻
update_consumption_mission(MissionItem, TaskConfig, {ConsumeType, BuyType, Count}) when is_record(MissionItem, mission_item) andalso is_record(TaskConfig, res_task) ->
  case TaskConfig#res_task.type_param_1 of
    30001 ->
      case ConsumeType of
        1 ->
          Progress = MissionItem#mission_item.mission_progress_1 + Count,
          {true, update_mission_finish_ts(TaskConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress}, TaskConfig)};
        _ ->  {false, MissionItem}     %%不是钻石
      end;
    30002 ->
      case ConsumeType of
        2 ->
          Progress = MissionItem#mission_item.mission_progress_1 + Count,
          {true, update_mission_finish_ts(TaskConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress}, TaskConfig)};
        _ ->
          {false, MissionItem}     %%不是金币
      end;
    30003 ->
      case ConsumeType of
        3 ->
          Progress = MissionItem#mission_item.mission_progress_1 + Count,
          {true, update_mission_finish_ts(TaskConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress}, TaskConfig)};
        _ -> {false, MissionItem}     %%不是果冻
      end;
    30004 -> %%购买道具（次数）
      case BuyType of
        3 ->   %%道具
          Progress = MissionItem#mission_item.mission_progress_1 + 1,
          {true, update_mission_finish_ts(TaskConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress}, TaskConfig)};
        4 ->   %%道具
          Progress = MissionItem#mission_item.mission_progress_1 + 1,
          {true, update_mission_finish_ts(TaskConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress}, TaskConfig)};
        _ -> {false, MissionItem}
      end;
    _ -> {false, MissionItem}
  end.

update_equipment_mission(MissionItem, MissionConfig, StrengthData) when is_record(MissionItem, mission_item) andalso is_record(MissionConfig, res_task) andalso is_record(StrengthData, strengthen_data) ->
  case MissionConfig#res_task.type_param_1 of
    50001 -> %%升级装备
      case StrengthData#strengthen_data.type of
        0 ->
          F = fun(Item, ItemID) -> ItemID =:= Item#record_item.key end,
          Record =
            case cache_util:find_item_by_id(MissionItem#mission_item.mission_record, F, StrengthData#strengthen_data.target_id) of
              fail ->  [#record_item{key = StrengthData#strengthen_data.target_id, value = 1} | MissionItem#mission_item.mission_record];
              {success, RecordItem} ->
                F1 = fun(Item, NItem) -> NItem#record_item.key =:= Item#record_item.key end,
                cache_util:update_list_item(MissionItem#mission_item.mission_record, F1, RecordItem#record_item{value = RecordItem#record_item.value + 1})
            end,
          case MissionConfig#res_task.type_param_2 of
            [] -> %%所有装备
              case MissionConfig#res_task.type_param_3 of
                "1" -> %%升级次数
                  Progress = MissionItem#mission_item.mission_progress_1 + 1,
                  {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress}, MissionConfig)};
                "2" -> %%升级装备个数
                  Progress = length(Record),
                  {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress, mission_record = Record}, MissionConfig)};
                _ -> %%不支持其他类型
                  ?FILE_LOG_WARNING("update_equipment_mission: upgrade all equipment mission type error", []),
                  {false, MissionItem}
              end;
            _EquipmentNo ->
              ?FILE_LOG_WARNING("update_equipment_mission: upgrade some equipment not support", []),
              {false, MissionItem}
          end;
        _ -> {false, MissionItem} %%不是升级
      end;
    50002 -> %%进阶装备
      case StrengthData#strengthen_data.type of
        1 ->
          case MissionConfig#res_task.type_param_2 of
            [] -> %%所有装备
             case MissionConfig#res_task.type_param_3 of
               "1" ->  %%所有装备进阶次数
                 Progress2 = MissionItem#mission_item.mission_progress_1 + 1,
                 {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress2}, MissionConfig)};
               "2" -> %%所有装备中 进阶某类品阶的装备个数
                 EquipmentConfig = cache_csv:get_equipment_config(StrengthData#strengthen_data.target_no),
                 StarLevel =  EquipmentConfig#res_equipment.star_level,
                 F = fun(Item, ItemID) -> ItemID =:= Item#record_item.key end,
                 Record =
                   case cache_util:find_item_by_id(MissionItem#mission_item.mission_record, F, StrengthData#strengthen_data.target_id) of
                     fail ->  [#record_item{key = StrengthData#strengthen_data.target_id, value = 1} | MissionItem#mission_item.mission_record];
                     {success, RecordItem} ->
                       F1 = fun(Item, NItem) -> NItem#record_item.key =:= Item#record_item.key end,
                       cache_util:update_list_item(MissionItem#mission_item.mission_record, F1, RecordItem#record_item{value = RecordItem#record_item.value + 1})
                   end,
                 case MissionConfig#res_task.condition_2 of
                   0 -> %%任意星阶
                     Progress_2 = 0,
                     Progress_1 = length(Record),
                     {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress_1, mission_progress_2 = Progress_2, mission_record = Record}, MissionConfig)};
                   StarLevel ->
                     Prog_1 = length(Record),
                     Prog_2 = EquipmentConfig#res_equipment.star_level,
                     {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Prog_1, mission_progress_2 = Prog_2, mission_record = Record}, MissionConfig)};
                   Other ->
                     ?FILE_LOG_DEBUG("update_equipment_mission=> equipment mission star level is ~p", [Other]),
                     {false, MissionItem}
                 end
             end;
            _EquipmentNo ->  %%某一个装备
              ?FILE_LOG_WARNING("update_equipment_mission => not support some equipment anvanced mission", []),
              {false, MissionItem}
          end;
        _ -> {false, MissionItem}     %%不是进阶
      end;
    50003 -> %%强化装备
      case MissionConfig#res_task.type_param_2 of
        [] -> %%所有装备
          case MissionConfig#res_task.type_param_3 of
            "1" ->  %%所有装备强化次数
              Progress2 = MissionItem#mission_item.mission_progress_1 + 1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress2}, MissionConfig)};
            "2" -> %%装备的个数
              F = fun(Item, ItemID) -> ItemID =:= Item#record_item.key end,
              Record =
                case cache_util:find_item_by_id(MissionItem#mission_item.mission_record, F, StrengthData#strengthen_data.target_id) of
                  fail ->  [#record_item{key = StrengthData#strengthen_data.target_id, value = 1} | MissionItem#mission_item.mission_record];
                  {success, RecordItem} ->
                    F1 = fun(Item, NItem) -> NItem#record_item.key =:= Item#record_item.key end,
                    cache_util:update_list_item(MissionItem#mission_item.mission_record, F1, RecordItem#record_item{value = RecordItem#record_item.value + 1})
                end,
                Progress_2 = 0,
                Progress_1 = length(Record),
                {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress_1, mission_progress_2 = Progress_2, mission_record = Record}, MissionConfig)}
          end;
        _EquipmentNo ->  %%某一个装备
          ?FILE_LOG_WARNING("update_equipment_mission => not support some equipment anvanced mission", []),
          {false, MissionItem}
      end
  end.

update_sweep_mission(MissionItem, MissionConfig, {TollgateID, SweepTimes}) when is_record(MissionItem, mission_item) andalso is_record(MissionConfig, res_task) ->
  lists:foldl(
    fun(_, {TmpFlag, TmpMissionItem}) ->
      {Flag, UpdateMission} = update_tollgate_mission(TmpMissionItem, MissionConfig, TollgateID),
      {Flag orelse TmpFlag, UpdateMission}
    end, {false, MissionItem}, lists:seq(1, SweepTimes)).
%%成功的关卡
update_tollgate_mission(MissionItem, MissionConfig, TollgateID) when is_record(MissionItem, mission_item) andalso is_record(MissionConfig, res_task) andalso is_integer(TollgateID) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  %%?FILE_LOG_DEBUG("update_tollgate_mission => task,config= ~p, tollgate config = ~p, gameEnd = ~p", [MissionConfig, TollgateConfig, GameEndData]),
  case MissionConfig#res_task.type_param_1 of
    20001 -> %%普通关卡
      if
        TollgateID < 10000 ->
          case TollgateConfig#res_stage.type of
            1 ->
              update_tollgate_mission_item("normal", TollgateConfig, MissionItem, MissionConfig);       %%普通关卡
            _ -> {false, MissionItem}
          end;
        true ->
          {false, MissionItem}
      end;
    20002 -> %%精英关卡
      if
        TollgateID < 10000 ->
          case TollgateConfig#res_stage.type of
            2 ->
              update_tollgate_mission_item("elite", TollgateConfig, MissionItem, MissionConfig);
            _ ->  {false, MissionItem}
          end;
        true ->                %%精英关卡
          {false, MissionItem}
      end;
    20006 -> %%boss关卡
      if
        TollgateID < 10000 ->
          case TollgateConfig#res_stage.type of
            3 ->
              update_tollgate_mission_item("boss", TollgateConfig, MissionItem, MissionConfig);
            _ ->
              {false, MissionItem}
          end;
        true ->                %%boss关卡
          {false, MissionItem}
      end;
    20004 -> %%任意关卡
      if
        TollgateID > 10000 -> {false, MissionItem};
        true ->
          update_tollgate_mission_item("any tollgate", TollgateConfig, MissionItem, MissionConfig)
      end;
    20005 -> %%活动关卡
      if
        TollgateID < 20000 -> {false, MissionItem};
        true ->                %%活动
          update_tollgate_mission_item("huodong", TollgateConfig, MissionItem, MissionConfig)
      end;
    20003 ->  %%无尽关卡
      if
        TollgateID < 10000 orelse TollgateID > 20000 -> {false, MissionItem};
        true ->
          update_tollgate_mission_item("endless", TollgateConfig, MissionItem, MissionConfig)
      end;
    20007 -> %%闯关模式
      if
        TollgateID > 10000 andalso TollgateID < 20000 -> {false, MissionItem};
        true ->
          update_tollgate_mission_item("storymode", TollgateConfig, MissionItem, MissionConfig)
      end
  end.

%%得分任务
update_tollgate_score_mission(MissionItem, MissionConfig, GameEndData) when is_record(MissionItem, mission_item) andalso is_record(MissionConfig, res_task) andalso is_record(GameEndData, game_end) ->
  case MissionConfig#res_task.type_param_1 of
    40001 ->   %%闯关累计得分
      if
        GameEndData#game_end.tollgate_id < 10000 ->     %%普通关卡
          GainScore = GameEndData#game_end.gain_score + MissionItem#mission_item.mission_progress_1,
          {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = GainScore}, MissionConfig)};
        true ->
          {false, MissionItem}
      end;
    40002 ->  %%无尽单局得分
      if
        GameEndData#game_end.tollgate_id > 10000 andalso GameEndData#game_end.tollgate_id < 20000 ->   %%无尽关卡
          Progress = MissionItem#mission_item.mission_progress_1,
          if
            GameEndData#game_end.gain_score > Progress ->
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = GameEndData#game_end.gain_score}, MissionConfig)};
            true -> {false, MissionItem}
          end;
        true ->
          {false, MissionItem}
      end;
    40003 ->  %%闯关单局得分
      if
        GameEndData#game_end.tollgate_id < 10000 ->
          PassProgress = MissionItem#mission_item.mission_progress_1,
          if
            GameEndData#game_end.gain_score > PassProgress ->
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = GameEndData#game_end.gain_score}, MissionConfig)};
            true -> {false, MissionItem}
          end;
        true -> {false, MissionItem}
      end
  end.

%%击杀任务
update_kill_monster_mission(MissionItem, MissionConfig, DataStatistics) when is_record(MissionItem, mission_item) andalso is_record(MissionConfig, res_task) andalso is_record(DataStatistics, game_end_statistics) ->
  case MissionConfig#res_task.type_param_1 of
    10001 ->   %%击杀普通怪物
      case MissionConfig#res_task.type_param_2 of
        [] -> %%所有普通怪物
          case MissionConfig#res_task.type_param_3 of
            [] ->   %%所有塔
              NormalData = DataStatistics#game_end_statistics.total_kill_normal_monster + MissionItem#mission_item.mission_progress_1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = NormalData}, MissionConfig)};
            TowerID -> %%某一塔的数据
              F =fun(Element, ID) -> Element#statistics_item.tower_id =:= ID end,
              case cache_util:find_item_by_id(DataStatistics#game_end_statistics.monster_list, F, TowerID) of
                fail ->
                  ?FILE_LOG_WARNING("update_kill_monster_mission=> game end data no tower id", []),
                  {false, MissionItem};
                {success, StatisticsItem} ->
                  NormalData1 = StatisticsItem#statistics_item.kill_monster#kill_monster.normal_monster_number + MissionItem#mission_item.mission_progress_1,
                  {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = NormalData1}, MissionConfig)}
              end
          end;
        ID -> %%某一类普通怪物
          case MissionConfig#res_task.type_param_3 of
            [] ->   %%所有塔
              %%统计某一类怪物的数量
              TotalNum = lists:foldr(fun(DataItem, TmpCount) ->
                Num = get_monster_number(DataItem#statistics_item.kill_monster#kill_monster.normal_monster_list, ID),
                Num + TmpCount
              end, 0, DataStatistics#game_end_statistics.monster_list),
              SomeMonsterData = TotalNum + MissionItem#mission_item.mission_progress_1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = SomeMonsterData}, MissionConfig)};

            TowerID -> %%某一塔的数据
              %%统计某一类怪物的数量某一个塔击杀的
              TotalNum = get_tower_kill_monster_number(normal, DataStatistics#game_end_statistics.monster_list, TowerID, ID),
              SomeMonsterData1 = TotalNum + MissionItem#mission_item.mission_progress_1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = SomeMonsterData1}, MissionConfig)}
          end
      end;
    10002 ->   %%击杀精英怪物
      case MissionConfig#res_task.type_param_2 of
        [] -> %%所有精英怪物
          case MissionConfig#res_task.type_param_3 of
            [] ->   %%所有塔
              NormalData = DataStatistics#game_end_statistics.total_kill_elite_monster + MissionItem#mission_item.mission_progress_1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = NormalData}, MissionConfig)};
            TowerID -> %%某一塔的数据
              F =fun(Element, ID) -> Element#statistics_item.tower_id =:= ID end,
              case cache_util:find_item_by_id(DataStatistics#game_end_statistics.monster_list, F, TowerID) of
                fail ->
                  ?FILE_LOG_WARNING("update_kill_monster_mission=> game end data no tower id", []),
                  {false, MissionItem};
                {success, StatisticsItem} ->
                  NormalData1 = StatisticsItem#statistics_item.kill_monster#kill_monster.elite_monster_number + MissionItem#mission_item.mission_progress_1,
                  {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = NormalData1}, MissionConfig)}
              end
          end;
        ID -> %%某一类精英怪物
          case MissionConfig#res_task.type_param_3 of
            [] ->   %%所有塔
              %%统计某一类怪物的数量
              TotalNum = lists:foldr(fun(DataItem, TmpCount) ->
                Num = get_monster_number(DataItem#statistics_item.kill_monster#kill_monster.elite_monster_list, ID),
                Num + TmpCount
              end, 0, DataStatistics#game_end_statistics.monster_list),
              SomeMonsterData = TotalNum + MissionItem#mission_item.mission_progress_1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = SomeMonsterData}, MissionConfig)};

            TowerID -> %%某一塔的数据
              %%统计某一类怪物的数量某一个塔击杀的
              TotalNum = get_tower_kill_monster_number(elite, DataStatistics#game_end_statistics.monster_list, TowerID, ID),
              SomeMonsterData1 = TotalNum + MissionItem#mission_item.mission_progress_1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = SomeMonsterData1}, MissionConfig)}
          end
      end;
    10003 ->   %%击杀boss怪物
      case MissionConfig#res_task.type_param_2 of
        [] -> %%所有boss怪物
          case MissionConfig#res_task.type_param_3 of
            [] ->   %%所有塔
              NormalData = DataStatistics#game_end_statistics.total_kill_boss_monster + MissionItem#mission_item.mission_progress_1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = NormalData}, MissionConfig)};
            TowerID -> %%某一塔的数据
              F =fun(Element, ID) -> Element#statistics_item.tower_id =:= ID end,
              case cache_util:find_item_by_id(DataStatistics#game_end_statistics.monster_list, F, TowerID) of
                fail ->
                  ?FILE_LOG_WARNING("update_kill_monster_mission=> game end data no tower id", []),
                  {false, MissionItem};
                {success, StatisticsItem} ->
                  NormalData1 = StatisticsItem#statistics_item.kill_monster#kill_monster.boss_monster_number + MissionItem#mission_item.mission_progress_1,
                  {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = NormalData1}, MissionConfig)}
              end
          end;
        ID -> %%某一类boss怪物
          case MissionConfig#res_task.type_param_3 of
            [] ->   %%所有塔
              %%统计某一类boss怪物的数量
              TotalNum = lists:foldr(fun(DataItem, TmpCount) ->
                Num = get_monster_number(DataItem#statistics_item.kill_monster#kill_monster.boss_monster_list, ID),
                Num + TmpCount
              end, 0, DataStatistics#game_end_statistics.monster_list),
              SomeMonsterData = TotalNum + MissionItem#mission_item.mission_progress_1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = SomeMonsterData}, MissionConfig)};

            TowerID -> %%某一塔的数据
              %%统计某一类boss怪物的数量某一个塔击杀的
              TotalNum = get_tower_kill_monster_number(boss, DataStatistics#game_end_statistics.monster_list, TowerID, ID),
              SomeMonsterData1 = TotalNum + MissionItem#mission_item.mission_progress_1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = SomeMonsterData1}, MissionConfig)}
          end
      end;
    10004 ->   %%击杀所有怪物
      case MissionConfig#res_task.type_param_3 of
        [] ->
          %%统计所有怪物数量
          AllMonsterNum = DataStatistics#game_end_statistics.total_kill_all_monster + MissionItem#mission_item.mission_progress_1,
          {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = AllMonsterNum}, MissionConfig)};
        TowerID ->
          F =fun(Element, ID) -> Element#statistics_item.tower_id =:= ID end,
          case cache_util:find_item_by_id(DataStatistics#game_end_statistics.monster_list, F, TowerID) of
            fail ->
              ?FILE_LOG_WARNING("update_kill_monster_mission=> game end data no tower id", []),
              {false, MissionItem};
            {success, StatisticsItem} ->
              AllMonsterNum1 = StatisticsItem#statistics_item.total_kill_monster + MissionItem#mission_item.mission_progress_1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = AllMonsterNum1}, MissionConfig)}
          end
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_tollgate_mission_item(_TollgateType, TollgateConfig, MissionItem, MissionConfig) ->
  TollgateID = TollgateConfig#res_stage.id,
  F = fun(Item, ItemID) -> ItemID =:= dd_util:to_integer(Item#record_item.key) end,
  Record =
    case cache_util:find_item_by_id(MissionItem#mission_item.mission_record, F, TollgateID) of
      fail ->  [#record_item{key = dd_util:to_list(TollgateID), value = 1} | MissionItem#mission_item.mission_record];
      {success, RecordItem} ->
        F1 = fun(Item, NItem) -> NItem#record_item.key =:= Item#record_item.key end,
        cache_util:update_list_item(MissionItem#mission_item.mission_record, F1, RecordItem#record_item{value = RecordItem#record_item.value + 1})
    end,
 %% ?FILE_LOG_DEBUG("mission config:~p", [MissionConfig]),
  case MissionConfig#res_task.type_param_2 of
    [] -> %%所有关卡
      case MissionConfig#res_task.type_param_3 of
        "2" -> %%通过多少关
          Progress1 = length(Record),
          {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress1, mission_record = Record}, MissionConfig)};
        "1" -> %%通关多少次,record中记录着通关记录
          Progress = MissionItem#mission_item.mission_progress_1 + 1,
          {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress, mission_record = Record}, MissionConfig)};
        _ ->
          ?FILE_LOG_DEBUG("error mission type", []),
          {false, MissionItem}
      end;
    TargetTollgateId  ->
      Id = dd_util:to_integer(TargetTollgateId),
      case Id of
        TollgateID ->
          case MissionConfig#res_task.type_param_3 of
            "1" -> %%通关多少次,record中记录着通关记录
              Progress = MissionItem#mission_item.mission_progress_1 + 1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress, mission_record = Record}, MissionConfig)};
            "2" ->
              Progress = MissionItem#mission_item.mission_progress_1 + 1,
              {true, update_mission_finish_ts(MissionConfig#res_task.type_param_1, MissionItem#mission_item{mission_progress_1 = Progress, mission_record = Record}, MissionConfig)};
            _ ->
              ?FILE_LOG_DEBUG("error mission type", []),
              {false, MissionItem}
          end;
        _ ->
          ?FILE_LOG_WARNING("not the mission tollgate ID", []),
          {false, MissionItem}
      end
  end.

get_monster_number(List, MonsterID) when is_list(List) andalso is_list(MonsterID) ->
  lists:foldr(fun({ID, N}, TmpNum) ->
      case ID of
        MonsterID -> TmpNum + N;
        _ -> TmpNum
      end
    end, 0, List).

get_tower_kill_monster_number(normal, List, TowerID, MonsterID) when is_list(List) andalso is_list(TowerID) andalso is_list(MonsterID) ->
  lists:foldr(fun(DataItem, TmpCount) ->
    case DataItem#statistics_item.tower_id of
      TowerID ->
        Num = get_monster_number(DataItem#statistics_item.kill_monster#kill_monster.normal_monster_list, MonsterID),
        Num + TmpCount;
      _ -> TmpCount
    end
  end, 0, List);
get_tower_kill_monster_number(elite, List, TowerID, MonsterID) when is_list(List) andalso is_list(TowerID) andalso is_list(MonsterID) ->
  lists:foldr(fun(DataItem, TmpCount) ->
    case DataItem#statistics_item.tower_id of
      TowerID ->
        Num = get_monster_number(DataItem#statistics_item.kill_monster#kill_monster.elite_monster_list, MonsterID),
        Num + TmpCount;
      _ -> TmpCount
    end
  end, 0, List);
get_tower_kill_monster_number(boss, List, TowerID, MonsterID) when is_list(List) andalso is_list(TowerID) andalso is_list(MonsterID) ->
  lists:foldr(fun(DataItem, TmpCount) ->
    case DataItem#statistics_item.tower_id of
      TowerID ->
        Num = get_monster_number(DataItem#statistics_item.kill_monster#kill_monster.boss_monster_list, MonsterID),
        Num + TmpCount;
      _ -> TmpCount
    end
  end, 0, List).

is_mission_finish(10001, MissionItem, MissionConfig) ->
  MissionItem#mission_item.mission_progress_1 >= MissionConfig#res_task.condition_1
    andalso MissionItem#mission_item.mission_progress_2 >= MissionConfig#res_task.condition_2;
is_mission_finish(_, MissionItem, MissionConfig) ->
  MissionItem#mission_item.mission_progress_1 >= MissionConfig#res_task.condition_1
    andalso MissionItem#mission_item.mission_progress_2 >= MissionConfig#res_task.condition_2.

update_mission_finish_ts(MissionTypeParam, MissionItem, MissionConfig) ->
  IsFinish = is_mission_finish(MissionTypeParam, MissionItem, MissionConfig),
  case IsFinish of
    true -> MissionItem#mission_item{mission_finish_ts = dd_util:timestamp()};
    false -> MissionItem
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   成就    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%过渡接口，用于修改当前玩家所有成绩数据，进行格式化处理
update_achievement_daily(Achievement) when is_record(Achievement, achievement) ->
  AchievementGroupList =
    lists:map(
      fun(AGroup) ->
        AGroup#achievement_group{
          item_level_1 = update_achievement_item_daily(AGroup#achievement_group.item_level_1),
          item_level_2 = update_achievement_item_daily(AGroup#achievement_group.item_level_2),
          item_level_3 = update_achievement_item_daily(AGroup#achievement_group.item_level_3)
        }
      end, Achievement#achievement.group_list),
  Achievement#achievement{group_list = AchievementGroupList}.

update_achievement_item_daily(AchievementItem) ->
  AchievementConfig = cache_csv:get_achievement_config(AchievementItem#achievement_item.id),
  update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem, AchievementConfig).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_achievement(InitAchievement) ->
  Achievement =
    InitAchievement#achievement{
    group_list = [],
    last_update_ts = dd_util:timestamp()
  },
  load_achievement_from_config(Achievement).

load_achievement_from_config(Achievement) when is_record(Achievement, achievement) ->
  AchievementList = cache_csv:get_all_achievement_config(),
  lists:foldl(
    fun(Element, TmpAchievement) ->
      InitItem = #achievement_item{id = Element#res_achievement.id, statistics  = 0, statistics_1 = 0, record = [], finish_ts = 0, get_reward_ts = 0, progress = 0},
      AchievementItem = update_achievement_finish_ts(Element#res_achievement.type_param_1, InitItem, Element),
      case cache_util:find_achievement_group_by_id(TmpAchievement#achievement.group_list,Element#res_achievement.group_id) of
        {success, Group} ->
          NGroup = insert_achievement(Group, AchievementItem, Element),
          NGroupList = cache_util:update_achievement_group(TmpAchievement#achievement.group_list, NGroup),
          TmpAchievement#achievement{group_list = NGroupList};
        fail ->
          GroupItem = insert_achievement(#achievement_group{group_id = Element#res_achievement.group_id}, AchievementItem, Element),
          OldIList = TmpAchievement#achievement.group_list,
          TmpAchievement#achievement{group_list = [GroupItem | OldIList], last_update_ts = dd_util:timestamp()}
      end
    end, Achievement, AchievementList).

reset_achievement_daily(Achievement) when is_record(Achievement, achievement) ->
  load_achievement_from_config(Achievement).

%% clear_non_existent_achievement(AchievementList) when is_list(AchievementList) ->
%%   clear_non_existent_achievement_1(AchievementList, []).
%% clear_non_existent_achievement_1([], OutList) -> OutList;
%% clear_non_existent_achievement_1([Group | T], OutList) ->
%%   case cache_csv:get_achievement_config_without_exception(Group#achievement_group.item_level_1#achievement_item.id) of
%%     {fail, _} -> clear_non_existent_achievement_1(T, OutList);
%%     {success, _} ->
%%        case cache_csv:get_achievement_config_without_exception(Group#achievement_group.item_level_2#achievement_item.id) of
%%          {fail, _} -> clear_non_existent_achievement_1(T, OutList);
%%          {success, _} ->
%%            case cache_csv:get_achievement_config_without_exception(Group#achievement_group.item_level_3#achievement_item.id) of
%%              {fail, _} -> clear_non_existent_achievement_1(T, OutList);
%%              {success, _} -> clear_non_existent_achievement_1(T, [Group | OutList])
%%            end
%%        end
%%   end.

insert_achievement(Group, AchievementItem, AchievementConfig) when is_record(Group, achievement_group) andalso is_record(AchievementItem, achievement_item) andalso is_record(AchievementConfig, res_achievement) ->
  case AchievementConfig#res_achievement.level of
    1 ->
      case Group#achievement_group.item_level_1 of
        undefined ->
          Group#achievement_group{item_level_1 = AchievementItem};
        _ ->
          if
            AchievementItem#achievement_item.id =:= Group#achievement_group.item_level_1#achievement_item.id -> Group;
            true ->  Group#achievement_group{item_level_1 = AchievementItem}
          end
      end;
    2 ->
      case Group#achievement_group.item_level_2 of
        undefined ->
          Group#achievement_group{item_level_2 = AchievementItem};
        _ ->
          if
            AchievementItem#achievement_item.id =:= Group#achievement_group.item_level_2#achievement_item.id -> Group;
            true ->  Group#achievement_group{item_level_2 = AchievementItem}
          end
      end;
    3 ->
      case Group#achievement_group.item_level_3 of
        undefined ->
          Group#achievement_group{item_level_3 = AchievementItem};
        _ ->
          if
            AchievementItem#achievement_item.id =:= Group#achievement_group.item_level_3#achievement_item.id -> Group;
            true ->  Group#achievement_group{item_level_3 = AchievementItem}
          end
      end;
    _Other ->
      ?FILE_LOG_ERROR("illegal achievement level", []),
      Group
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%成就%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_achievement(rank, Uin, Achievement, {CurFriendRank, CurServerRank}) when is_record(Achievement, achievement)->
  {GroupList, UpdateList} = update_achievement_list(Achievement#achievement.group_list, Uin, fun update_achievement_item_with_rank_data/2, {CurFriendRank, CurServerRank}),
  {UpdateList, Achievement#achievement{group_list = GroupList}};
update_achievement(game_end, Uin, Achievement, GameEndData) when is_record(Achievement, achievement) andalso is_record(GameEndData, game_end)->
  {GroupList, UpdateList} = update_achievement_list(Achievement#achievement.group_list, Uin, fun update_achievement_item_with_game_end_data/2, GameEndData),
  {UpdateList, Achievement#achievement{group_list = GroupList}};
update_achievement(strengthen, Uin, Achievement, StrengthenData) when is_record(Achievement, achievement) andalso is_record(StrengthenData, strengthen_data) ->
  {GroupList, UpdateList} = update_achievement_list(Achievement#achievement.group_list, Uin, fun update_achievement_item_with_strengthen_data/2, StrengthenData),
  {UpdateList, Achievement#achievement{group_list = GroupList}};
update_achievement(strength_gift, Uin, Achievement, {Type, StrengthCount}) ->  %%体力赠送和接受 , 1表示赠送，2表示接受
  {GroupList, UpdateList} = update_achievement_list(Achievement#achievement.group_list, Uin, fun update_achievement_item_with_strength_gift/2, {Type, StrengthCount}),
  {UpdateList, Achievement#achievement{group_list = GroupList}};
update_achievement(friend_number, Uin, Achievement, {Uin, FriendNum}) ->
  {GroupList, UpdateList} = update_achievement_list(Achievement#achievement.group_list, Uin, fun update_achievement_item_with_friend_data/2, {Uin, FriendNum}),
  {UpdateList, Achievement#achievement{group_list = GroupList}};
update_achievement(harvest_obstacle, Uin, Achievement, WBId) ->
  {GroupList, UpdateList} = update_achievement_list(Achievement#achievement.group_list, Uin, fun update_achievement_item_with_harvest_obstacle/2, WBId),
  {UpdateList, Achievement#achievement{group_list = GroupList}}.


update_achievement_list(GroupList, Uin, Fun, Param) ->
  update_achievement_list_1(GroupList, Uin, Fun, Param, [], []).
update_achievement_list_1([], _, _, _, OutList, UpdateGroupList) -> {OutList, UpdateGroupList};
update_achievement_list_1([AchievementGroup | T], Uin, Fun, Param, OutList, UpdateGroupList) ->
  {IsUpdateLevel1, ItemLevel1} = Fun(AchievementGroup#achievement_group.item_level_1, Param),
  achievement_log_flow(IsUpdateLevel1, Uin, AchievementGroup#achievement_group.item_level_1, ItemLevel1),
  {IsUpdateLevel2, ItemLevel2} = Fun(AchievementGroup#achievement_group.item_level_2, Param),
  achievement_log_flow(IsUpdateLevel2, Uin, AchievementGroup#achievement_group.item_level_2, ItemLevel2),
  {IsUpdateLevel3, ItemLevel3} = Fun(AchievementGroup#achievement_group.item_level_3, Param),
  achievement_log_flow(IsUpdateLevel3, Uin, AchievementGroup#achievement_group.item_level_3, ItemLevel3),
  IsUpdate = IsUpdateLevel1 orelse IsUpdateLevel2 orelse IsUpdateLevel3,
  case IsUpdate of
    true ->
      NGroup = AchievementGroup#achievement_group{item_level_1 = ItemLevel1, item_level_2 = ItemLevel2, item_level_3 = ItemLevel3},
      update_achievement_list_1(T, Uin, Fun, Param, [NGroup | OutList], [NGroup | UpdateGroupList]);
    false ->
      update_achievement_list_1(T, Uin, Fun, Param, [AchievementGroup | OutList], UpdateGroupList)
  end.

update_achievement_item_with_harvest_obstacle(AchievementItem, WBId) when is_record(AchievementItem, achievement_item) andalso is_list(WBId) ->
  AchievementConfig = cache_csv:get_achievement_config(AchievementItem#achievement_item.id),
  AchievementType = AchievementConfig#res_achievement.type_param_1,
  if
    AchievementType > 60000 andalso AchievementType < 70000 -> %%可收集障碍物
      update_harvest_obstacle_achievement(AchievementItem, AchievementConfig, WBId);
    true ->
      {false, AchievementItem}
  end.

update_achievement_item_with_friend_data(AchievementItem, {Uin, FriendNum}) when is_record(AchievementItem, achievement_item) andalso is_integer(Uin) andalso is_integer(FriendNum) andalso FriendNum > 0 ->
  AchievementConfig = cache_csv:get_achievement_config(AchievementItem#achievement_item.id),
  AchievementType = AchievementConfig#res_achievement.type_param_1,
  if
    AchievementType > 70000 andalso AchievementType < 80000 -> %%好友数量
      update_friend_achievement(AchievementItem, AchievementConfig, {Uin, FriendNum});
    true ->
      {false, AchievementItem}
  end.

update_achievement_item_with_strength_gift(AchievementItem, {Type, Count}) when is_record(AchievementItem, achievement_item) andalso is_integer(Type) andalso is_integer(Count) ->
  AchievementConfig = cache_csv:get_achievement_config(AchievementItem#achievement_item.id),
  AchievementType = AchievementConfig#res_achievement.type_param_1,
  if
    AchievementType > 70000 andalso AchievementType < 80000 -> %%体力赠送接受
      update_strength_gift_achievement(AchievementItem, AchievementConfig, {Type, Count});
    true ->
      {false, AchievementItem}
  end.

update_achievement_item_with_rank_data(AchievementItem, {CurFriendRank, CurServerRank}) when is_record(AchievementItem, achievement_item) ->
  AchievementConfig = cache_csv:get_achievement_config(AchievementItem#achievement_item.id),
  AchievementType = AchievementConfig#res_achievement.type_param_1,
  if
    AchievementType > 80000 andalso AchievementType < 90000 -> %%无尽分数及排名
      update_ranking_achievement(AchievementItem, AchievementConfig, {CurFriendRank, CurServerRank});
    true ->
      {false, AchievementItem}
  end.

update_achievement_item_with_game_end_data(AchievementItem, GameEndData) when is_record(AchievementItem, achievement_item) andalso is_record(GameEndData, game_end) ->
  AchievementConfig = cache_csv:get_achievement_config(AchievementItem#achievement_item.id),
  AchievementType = AchievementConfig#res_achievement.type_param_1,
  if
    AchievementType > 10000 andalso AchievementType < 20000 -> %%杀怪成就
      update_kill_monster_achievement(AchievementItem, AchievementConfig, GameEndData#game_end.data_statistics);
    AchievementType > 20000 andalso AchievementType < 30000 andalso GameEndData#game_end.success >= 0-> %%过关成就
      update_tollgate_achievement(AchievementItem, AchievementConfig, GameEndData);
    AchievementType > 30000 andalso AchievementType < 40000 -> %%种塔成就
      update_tower_related_achievement(AchievementItem, AchievementConfig, GameEndData);
    AchievementType > 40000 andalso AchievementType < 50000 ->  %%使用技能的成就
      update_tower_related_achievement(AchievementItem, AchievementConfig, GameEndData);
    AchievementType > 80000 andalso AchievementType < 90000 -> %%无尽分数及排名
      update_tollgate_score_achievement(AchievementItem, AchievementConfig, {GameEndData#game_end.tollgate_id, GameEndData#game_end.gain_score});
    true ->
      {false, AchievementItem}
  end.

update_achievement_item_with_strengthen_data(AchievementItem, StrengthData) when is_record(AchievementItem, achievement_item) andalso is_record(StrengthData, strengthen_data) ->
  AchievementConfig = cache_csv:get_achievement_config(AchievementItem#achievement_item.id),
  AchievementType = AchievementConfig#res_achievement.type_param_1,
  if
    AchievementType > 50000 andalso AchievementType < 60000 -> %%杀怪成就
      update_equipment_achievement(AchievementItem, AchievementConfig, StrengthData);
    true ->
      {false, AchievementItem}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_harvest_obstacle_achievement(AchievementItem, AchievementConfig, _WBId) ->
  case AchievementConfig#res_achievement.type_param_1 of
    60001 -> %%好友数量
      if
        AchievementItem#achievement_item.finish_ts > 0 -> {false, AchievementItem};
        true ->
          Progress = AchievementItem#achievement_item.statistics + 1,
          {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress}, AchievementConfig)}
      end;
    _ -> {false, AchievementItem}
  end.

update_friend_achievement(AchievementItem, AchievementConfig, {_, FriendNum}) ->
  case AchievementConfig#res_achievement.type_param_1 of
    70001 -> %%好友数量
      if
        AchievementItem#achievement_item.finish_ts > 0 -> {false, AchievementItem};
        true ->
          {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = FriendNum}, AchievementConfig)}
      end;
    _ -> {false, AchievementItem}
  end.

update_strength_gift_achievement(AchievementItem, AchievementConfig, {Type, _Count}) ->
  case AchievementConfig#res_achievement.type_param_1 of
    70003 -> %%赠送好友体力次数
      if
        Type =/= 1 -> {false, AchievementItem};
        AchievementItem#achievement_item.finish_ts > 0 -> {false, AchievementItem};
        true ->
          Progress = AchievementItem#achievement_item.statistics + 1,
          {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress}, AchievementConfig)}
      end;
    70002 ->
      if
        Type =/= 2 -> {false, AchievementItem};
        AchievementItem#achievement_item.finish_ts > 0 -> {false, AchievementItem};
        true ->
          Progress = AchievementItem#achievement_item.statistics + 1,
          {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress}, AchievementConfig)}
      end;
    _Other -> {false, AchievementItem}
  end.

update_ranking_achievement(AchievementItem, AchievementConfig, {CurFriendRank, CurServerRank})  ->
  case AchievementConfig#res_achievement.type_param_1 of
    80001 ->        %%好友排名
      if
        AchievementItem#achievement_item.finish_ts > 0 -> {false, AchievementItem};
        CurFriendRank < 0 -> {false, AchievementItem};
        true ->
          {true, update_achievement_finish_ts(80001, AchievementItem#achievement_item{statistics_1 = CurFriendRank}, AchievementConfig)}
      end;
    80003 ->     %%全服排名
      if
        AchievementItem#achievement_item.finish_ts > 0 -> {false, AchievementItem};
        CurServerRank < 0 -> {false, AchievementItem};
        true ->
          {true, update_achievement_finish_ts(80003, AchievementItem#achievement_item{statistics_1 = CurServerRank}, AchievementConfig)}
      end;
    _Other -> {false, AchievementItem}
  end.

update_equipment_achievement(AchievementItem, AchievementConfig, StrengthData) when is_record(AchievementItem, achievement_item) andalso is_record(AchievementConfig, res_achievement) andalso is_record(StrengthData, strengthen_data) ->
  case AchievementConfig#res_achievement.type_param_1 of
    50001 -> %%升级装备
      if
        StrengthData#strengthen_data.type =/= 0 -> {false, AchievementItem}; %%不是升级
        true ->
          case AchievementConfig#res_achievement.type_param_2 of
            [] -> %%所有装备
              case AchievementConfig#res_achievement.type_param_3 of
                "1" -> %%升级次数
                  Progress = AchievementItem#achievement_item.statistics + 1,
                  case AchievementItem#achievement_item.finish_ts of
                    0 ->
                      {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress}, AchievementConfig)};
                    _  ->
                      {false, AchievementItem#achievement_item{statistics = Progress}}
                  end;
                _ -> %%不支持其他类型
                  ?FILE_LOG_WARNING("update_equipment_achievement: upgrade all equipment achievement type error", []),
                  {false, AchievementItem}
              end;
            _EquipmentNo ->
              ?FILE_LOG_WARNING("update_equipment_achievement: upgrade some equipment not support", []),
              {false, AchievementItem}
          end
      end;
    50002 -> %%进阶装备
      if
        StrengthData#strengthen_data.type =/= 1 -> {false, AchievementItem}; %%不是进阶
        true ->
          case AchievementConfig#res_achievement.type_param_2 of
            [] -> %%所有装备
              case AchievementConfig#res_achievement.type_param_3 of
                "1" ->  %%所有装备进阶次数
                  Progress2 = AchievementItem#achievement_item.statistics + 1,
                  case AchievementItem#achievement_item.finish_ts of
                    0 ->
                      {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress2}, AchievementConfig)};
                    _ ->
                      {false, AchievementItem#achievement_item{statistics = Progress2}}
                  end;
                "2" -> %%所有装备中 进阶某类品阶的装备个数
                  EquipmentConfig = cache_csv:get_equipment_config(StrengthData#strengthen_data.target_no),
                  _StarLevel =  EquipmentConfig#res_equipment.star_level,
                  F = fun(Item, ItemID) -> ItemID =:= Item#record_item.key end,
                  Record =
                    case cache_util:find_item_by_id(AchievementItem#achievement_item.record, F, StrengthData#strengthen_data.target_id) of
                      fail ->  [#record_item{key = StrengthData#strengthen_data.target_id, value = 1} | AchievementItem#achievement_item.record];
                      {success, RecordItem} ->
                        F1 = fun(Item, NItem) -> NItem#record_item.key =:= Item#record_item.key end,
                        cache_util:update_list_item(AchievementItem#achievement_item.record, F1, RecordItem#record_item{value = RecordItem#record_item.value + 1})
                    end,
                  case AchievementConfig#res_achievement.condition_2 of
                    0 -> %%任意星阶
                      Progress_1 = AchievementItem#achievement_item.statistics + 1,
                      case AchievementItem#achievement_item.finish_ts of
                        0 ->
                          {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress_1, record = Record}, AchievementConfig)};
                        _ ->
                          {false, AchievementItem#achievement_item{statistics = Progress_1, record = Record}}
                      end;
%%                     StarLevel ->
%%                       Prog_1 = AchievementItem#achievement_item.statistics + 1,
%%                       Prog_2 = EquipmentConfig#res_equipment.star_level,
%%                       case AchievementConfig#achievement_item.finish_ts of
%%                         0 ->
%%                           {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Prog_1, statistics_1 = Prog_2, record = Record}, AchievementConfig)};
%%                         _ ->
%%                           {false, AchievementItem#achievement_item{statistics = Prog_1, statistics_1 = Prog_2, record = Record}}
%%                       end;
                    Other ->
                      ?FILE_LOG_DEBUG("update_equipment_achievement=> equipment achievement star level is ~p", [Other]),
                      {false, AchievementItem}
                  end;
                _ ->
                 %% ?FILE_LOG_DEBUG("error achievement type, [~p]", AchievementConfig),
                  {false, AchievementItem}
              end;
            _EquipmentNo ->
              %%某一个装备
              ?FILE_LOG_WARNING("update_equipment_achievement: advanced some equipment not support", []),
              {false, AchievementItem}
          end
      end;
    50003 -> %%强化装备（升级进阶）
      case AchievementConfig#res_achievement.type_param_2 of
        [] -> %%所有装备
          case AchievementConfig#res_achievement.type_param_3 of
            "1" ->  %%所有装备强化次数
              Progress2 = AchievementItem#achievement_item.statistics + 1,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress2}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = Progress2}}
              end;
            "2" -> %%所有装备强化个数
              F = fun(Item, ItemID) -> ItemID =:= Item#record_item.key end,
              Record =
                case cache_util:find_item_by_id(AchievementItem#achievement_item.record, F, StrengthData#strengthen_data.target_id) of
                  fail ->  [#record_item{key = StrengthData#strengthen_data.target_id, value = 1} | AchievementItem#achievement_item.record];
                  {success, RecordItem} ->
                    F1 = fun(Item, NItem) -> NItem#record_item.key =:= Item#record_item.key end,
                    cache_util:update_list_item(AchievementItem#achievement_item.record, F1, RecordItem#record_item{value = RecordItem#record_item.value + 1})
                end,
                Progress_1 = length(Record),
                Progress_2 = 0,
                case AchievementConfig#achievement_item.finish_ts of
                  0 ->
                    {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress_1, statistics_1 = Progress_2, record = Record}, AchievementConfig)};
                  _ ->
                    {false, AchievementItem#achievement_item{statistics = Progress_1, statistics_1 = Progress_2, record = Record}}
                end;
            _ ->
              %%?FILE_LOG_DEBUG("error achievement type, [~p]", AchievementConfig),
              {false, AchievementItem}
          end;
        _EquipmentNo ->
          %%某一个装备
          ?FILE_LOG_WARNING("update_equipment_achievement: advanced some equipment not support", []),
          {false, AchievementItem}
      end;
    _ -> {false, AchievementItem}
  end.

%%更新于塔相关的成就
update_tower_related_achievement(AchievementItem, AchievementConfig, GameEndData) when is_record(AchievementItem, achievement_item) andalso is_record(AchievementConfig, res_achievement) andalso is_record(GameEndData, game_end) ->
  case AchievementConfig#res_achievement.type_param_1 of
    30001 -> %%造塔统计
      update_tower_achievement_item(build_tower, AchievementItem, AchievementConfig, GameEndData);
    40001 ->
      update_tower_achievement_item(use_skill,  AchievementItem, AchievementConfig, GameEndData);
    _ -> {false, AchievementItem}
  end.

update_tower_achievement_item(build_tower, AchievementItem, AchievementConfig, GameEndData) ->
  case AchievementConfig#res_achievement.type_param_3 of
    [] -> %%所有塔
      Count = AchievementItem#achievement_item.statistics + GameEndData#game_end.data_statistics#game_end_statistics.total_build_tower,
      case AchievementItem#achievement_item.finish_ts of
        0 ->
          {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Count}, AchievementConfig)};
        _ ->
          {false, AchievementItem#achievement_item{statistics = Count}}
      end;
    TowerID -> %%建造某个塔
      F = fun(Item, ID) -> Item#statistics_item.tower_id =:= ID end,
      case cache_util:find_item_by_id(GameEndData#game_end.data_statistics#game_end_statistics.monster_list, F, TowerID) of
        {success, StatisticItem} ->
          TCount = AchievementItem#achievement_item.statistics + StatisticItem#statistics_item.build_num,
          case AchievementItem#achievement_item.finish_ts of
            0 ->
              {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = TCount}, AchievementConfig)};
            _ ->
              {false, AchievementItem#achievement_item{statistics = TCount}}
          end;
        fail -> {false, AchievementItem}
      end
  end;
update_tower_achievement_item(use_skill, AchievementItem, AchievementConfig, GameEndData) ->
  case AchievementConfig#res_achievement.type_param_3 of
    [] -> %%所有塔
      Count = AchievementItem#achievement_item.statistics + GameEndData#game_end.data_statistics#game_end_statistics.total_use_skill,
      case AchievementItem#achievement_item.finish_ts of
        0 ->
          {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Count}, AchievementConfig)};
        _ ->
          {false, AchievementItem#achievement_item{statistics = Count}}
      end;
    TowerID -> %%建造某个塔
      F = fun(Item, ID) -> Item#statistics_item.tower_id =:= ID end,
      case cache_util:find_item_by_id(GameEndData#game_end.data_statistics#game_end_statistics.monster_list, F, TowerID) of
        {success, StatisticItem} ->
          TCount = AchievementItem#achievement_item.statistics + StatisticItem#statistics_item.use_skill_num,
          case AchievementItem#achievement_item.finish_ts of
            0 ->
              {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = TCount}, AchievementConfig)};
            _ ->
              {false, AchievementItem#achievement_item{statistics = TCount}}
          end;
        fail -> {false, AchievementItem}
      end
  end.

%%成功的关卡
update_tollgate_achievement(AchievementItem, AchievementConfig, GameEndData) when is_record(AchievementItem, achievement_item) andalso is_record(AchievementConfig, res_achievement) andalso is_record(GameEndData, game_end) ->
  TollgateID = GameEndData#game_end.tollgate_id,
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  case AchievementConfig#res_achievement.type_param_1 of
    20001 -> %%过关统计
      if
        TollgateID > 10000  -> {false, AchievementItem};
        true ->
          update_tollgate_achievement_item(times, "any tollgate", TollgateConfig, AchievementItem, AchievementConfig, GameEndData)
      end;
    20002 -> %%全部普通关卡通关
      if
        TollgateID > 10000 orelse TollgateConfig#res_stage.type =/= 1 -> {false, AchievementItem};
        true ->
          update_tollgate_achievement_item(times, "any normal tollgate", TollgateConfig, AchievementItem, AchievementConfig, GameEndData)
      end;
    20005 ->  %%全部精英关卡
      if
        TollgateID > 10000 orelse TollgateConfig#res_stage.type =/= 2 -> {false, AchievementItem};
        true ->
          update_tollgate_achievement_item(times, "any elite tollgate", TollgateConfig, AchievementItem, AchievementConfig, GameEndData)
      end;
    20003 ->  %%获得星星
      if
        TollgateID > 10000 ->  {false, AchievementItem};
        true ->
          update_tollgate_achievement_item(star, "any tollgate", TollgateConfig, AchievementItem, AchievementConfig, GameEndData)
      end;
    20004 ->  %%摧毁障碍物
      update_tollgate_achievement_item(obstacle, "any tollgate", TollgateConfig, AchievementItem, AchievementConfig, GameEndData);
    _ -> {false, AchievementItem}
  end.

%%得分任务
update_tollgate_score_achievement(AchievementItem, AchievementConfig, {TollgateID, GainScore}) when is_record(AchievementItem, achievement_item) andalso is_record(AchievementConfig, res_achievement) ->
  case AchievementConfig#res_achievement.type_param_1 of
    80002 ->   %%闯关累计得分
      if
        TollgateID > 10000 andalso TollgateID < 20000 ->     %%无尽分数累积
          Score = GainScore + AchievementItem#achievement_item.statistics,
          case AchievementItem#achievement_item.finish_ts of
            0 ->
              {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Score}, AchievementConfig)};
            _ ->
              {false, AchievementItem#achievement_item{statistics = Score}}
          end;
        true ->
          {false, AchievementItem}
      end;
    _ -> {false, AchievementItem}
  end.

%%击杀任务
update_kill_monster_achievement(AchievementItem, AchievementConfig, DataStatistics) when is_record(AchievementItem, achievement_item) andalso is_record(AchievementConfig, res_achievement) andalso is_record(DataStatistics, game_end_statistics) ->
  case AchievementConfig#res_achievement.type_param_1 of
    10001 ->   %%击杀普通怪物
      case AchievementConfig#res_achievement.type_param_2 of
        [] -> %%所有普通怪物
          case AchievementConfig#res_achievement.type_param_3 of
            [] ->   %%所有塔
              NormalData = DataStatistics#game_end_statistics.total_kill_normal_monster + AchievementItem#achievement_item.statistics,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = NormalData}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = NormalData}}
              end;
            TowerID -> %%某一塔的数据
              F =fun(Element, ID) -> Element#statistics_item.tower_id =:= ID end,
              case cache_util:find_item_by_id(DataStatistics#game_end_statistics.monster_list, F, TowerID) of
                fail ->
                  ?FILE_LOG_WARNING("update_kill_monster_achievement=> game end data no tower id", []),
                  {false, AchievementItem};
                {success, StatisticsItem} ->
                  NormalData1 = StatisticsItem#statistics_item.kill_monster#kill_monster.normal_monster_number + AchievementItem#achievement_item.statistics,
                  case AchievementItem#achievement_item.finish_ts of
                    0 ->
                      {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = NormalData1}, AchievementConfig)};
                    _ ->
                      {false, AchievementItem#achievement_item{statistics = NormalData1}}
                  end
              end
          end;
        ID -> %%某一类普通怪物
          case AchievementConfig#res_achievement.type_param_3 of
            [] ->   %%所有塔
              %%统计某一类怪物的数量
              TotalNum = lists:foldr(fun(DataItem, TmpCount) ->
                Num = get_monster_number(DataItem#statistics_item.kill_monster#kill_monster.normal_monster_list, ID),
                Num + TmpCount
              end, 0, DataStatistics#game_end_statistics.monster_list),
              SomeMonsterData = TotalNum + AchievementItem#achievement_item.statistics,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = SomeMonsterData}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = SomeMonsterData}}
              end;

            TowerID -> %%某一塔的数据
              %%统计某一类怪物的数量某一个塔击杀的
              TotalNum = get_tower_kill_monster_number(normal, DataStatistics#game_end_statistics.monster_list, TowerID, ID),
              SomeMonsterData1 = TotalNum + AchievementItem#achievement_item.statistics,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = SomeMonsterData1}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = SomeMonsterData1}}
              end
          end
      end;
    10002 ->   %%击杀精英怪物
      case AchievementConfig#res_achievement.type_param_2 of
        [] -> %%所有精英怪物
          case AchievementConfig#res_achievement.type_param_3 of
            [] ->   %%所有塔
              NormalData = DataStatistics#game_end_statistics.total_kill_elite_monster + AchievementItem#achievement_item.statistics,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = NormalData}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = NormalData}}
              end;
            TowerID -> %%某一塔的数据
              F =fun(Element, ID) -> Element#statistics_item.tower_id =:= ID end,
              case cache_util:find_item_by_id(DataStatistics#game_end_statistics.monster_list, F, TowerID) of
                fail ->
                  ?FILE_LOG_WARNING("update_kill_monster_achievement => game end data no tower id", []),
                  {false, AchievementItem};
                {success, StatisticsItem} ->
                  NormalData1 = StatisticsItem#statistics_item.kill_monster#kill_monster.elite_monster_number + AchievementItem#achievement_item.statistics,
                  case AchievementItem#achievement_item.finish_ts of
                    0 ->
                     {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = NormalData1}, AchievementConfig)};
                    _ ->
                      {false, AchievementItem#achievement_item{statistics = NormalData1}}
                  end
              end
          end;
        ID -> %%某一类精英怪物
          case AchievementConfig#res_achievement.type_param_3 of
            [] ->   %%所有塔
              %%统计某一类怪物的数量
              TotalNum = lists:foldr(fun(DataItem, TmpCount) ->
                Num = get_monster_number(DataItem#statistics_item.kill_monster#kill_monster.elite_monster_list, ID),
                Num + TmpCount
              end, 0, DataStatistics#game_end_statistics.monster_list),
              SomeMonsterData = TotalNum + AchievementItem#achievement_item.statistics,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = SomeMonsterData}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = SomeMonsterData}}
              end;

            TowerID -> %%某一塔的数据
              %%统计某一类怪物的数量某一个塔击杀的
              TotalNum = get_tower_kill_monster_number(elite, DataStatistics#game_end_statistics.monster_list, TowerID, ID),
              SomeMonsterData1 = TotalNum + AchievementItem#achievement_item.statistics,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = SomeMonsterData1}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = SomeMonsterData1}}
              end
          end
      end;
    10003 ->   %%击杀boss怪物
      case AchievementConfig#res_achievement.type_param_2 of
        [] -> %%所有boss怪物
          case AchievementConfig#res_achievement.type_param_3 of
            [] ->   %%所有塔
              NormalData = DataStatistics#game_end_statistics.total_kill_boss_monster + AchievementItem#achievement_item.statistics,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = NormalData}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = NormalData}}
              end;
            TowerID -> %%某一塔的数据
              F =fun(Element, ID) -> Element#statistics_item.tower_id =:= ID end,
              case cache_util:find_item_by_id(DataStatistics#game_end_statistics.monster_list, F, TowerID) of
                fail ->
                  ?FILE_LOG_WARNING("update_kill_monster_achievement=> game end data no tower id", []),
                  {false, AchievementItem};
                {success, StatisticsItem} ->
                  NormalData1 = StatisticsItem#statistics_item.kill_monster#kill_monster.boss_monster_number + AchievementItem#achievement_item.statistics,
                  case AchievementItem#achievement_item.finish_ts of
                    0 ->
                      {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = NormalData1}, AchievementConfig)};
                    _ ->
                      {false, AchievementItem#achievement_item{statistics = NormalData1}}
                  end
              end
          end;
        ID -> %%某一类boss怪物
          case AchievementConfig#res_achievement.type_param_3 of
            [] ->   %%所有塔
              %%统计某一类boss怪物的数量
              TotalNum = lists:foldr(fun(DataItem, TmpCount) ->
                Num = get_monster_number(DataItem#statistics_item.kill_monster#kill_monster.boss_monster_list, ID),
                Num + TmpCount
              end, 0, DataStatistics#game_end_statistics.monster_list),
              SomeMonsterData = TotalNum + AchievementItem#achievement_item.statistics,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = SomeMonsterData}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = SomeMonsterData}}
              end;

            TowerID -> %%某一塔的数据
              %%统计某一类boss怪物的数量某一个塔击杀的
              TotalNum = get_tower_kill_monster_number(boss, DataStatistics#game_end_statistics.monster_list, TowerID, ID),
              SomeMonsterData1 = TotalNum + AchievementItem#achievement_item.statistics,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = SomeMonsterData1}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = SomeMonsterData1}}
              end
          end
      end;
    10004 ->   %%击杀所有怪物
      case AchievementConfig#res_achievement.type_param_3 of
        [] ->
          %%统计所有怪物数量
          AllMonsterNum = DataStatistics#game_end_statistics.total_kill_all_monster + AchievementItem#achievement_item.statistics,
          case AchievementItem#achievement_item.finish_ts of
            0 ->
              {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = AllMonsterNum}, AchievementConfig)};
            _ ->
              {false, AchievementItem#achievement_item{statistics = AllMonsterNum}}
          end;
        TowerID ->
          F =fun(Element, ID) -> Element#statistics_item.tower_id =:= ID end,
          case cache_util:find_item_by_id(DataStatistics#game_end_statistics.monster_list, F, dd_util:to_list(TowerID)) of
            fail ->
              ?FILE_LOG_WARNING("update_kill_monster_achievement=> game end data no tower id, id = ~p", [TowerID]),
              {false, AchievementItem};
            {success, StatisticsItem} ->
              ?FILE_LOG_DEBUG("update_kill_monster_achievement=> achievement id = ~p, achievement = ~p, statistics = ~p", [AchievementItem#achievement_item.id, AchievementItem, StatisticsItem]),
              AllMonsterNum1 = StatisticsItem#statistics_item.total_kill_monster + AchievementItem#achievement_item.statistics,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = AllMonsterNum1}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = AllMonsterNum1}}
              end
          end
      end;
    _ -> {false, AchievementItem}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_tollgate_achievement_item(times, TollgateType, TollgateConfig, AchievementItem, AchievementConfig, _EndData) ->
  TollgateID = TollgateConfig#res_stage.id,
  F = fun(Item, ItemID) -> ItemID =:= dd_util:to_integer(Item#record_item.key) end,
  Record =
    case cache_util:find_item_by_id(AchievementItem#achievement_item.record, F, TollgateID) of
      fail ->  [#record_item{key = dd_util:to_list(TollgateID), value = 1} | AchievementItem#achievement_item.record];
      {success, RecordItem} ->
        F1 = fun(Item, NItem) -> NItem#record_item.key =:= Item#record_item.key end,
        cache_util:update_list_item( AchievementItem#achievement_item.record, F1, RecordItem#record_item{value = RecordItem#record_item.value + 1})
    end,
  case AchievementConfig#res_achievement.type_param_2 of
    [] -> %%所有关卡
      case AchievementConfig#res_achievement.type_param_3 of
        "1" -> %%通关多少次,record中记录着通关记录
          Progress = AchievementItem#achievement_item.statistics + 1,
          case AchievementItem#achievement_item.finish_ts of
            0 ->
              {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress, record = Record}, AchievementConfig)};
            _ ->
              {false, AchievementItem#achievement_item{statistics = Progress, record = Record}}
          end;
        "2" -> %%通过多少关
          Progress1 = length(Record),
          case AchievementItem#achievement_item.finish_ts of
            0 ->
              {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress1, record = Record}, AchievementConfig)};
            _ ->
              {false, AchievementItem#achievement_item{statistics = Progress1, record = Record}}
          end;
        _ ->
         %% ?FILE_LOG_DEBUG("error achievement type, [~p]", [AchievementConfig]),
          {false, AchievementItem}
      end;
    TargetTollgateId  ->
      Id = dd_util:to_integer(TargetTollgateId),
     case Id of
       TollgateID ->
          case AchievementConfig#res_achievement.type_param_3 of
            "1" -> %%通关多少次,record中记录着通关记录
              Progress = AchievementItem#achievement_item.statistics + 1,
              case AchievementItem#achievement_item.finish_ts of
                0 ->
                  {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = Progress, record = Record}, AchievementConfig)};
                _ ->
                  {false, AchievementItem#achievement_item{statistics = Progress, record = Record}}
              end;
            _ -> %%通过多少关
              ?FILE_LOG_ERROR("~p tollgate can not pass more, tollgate id = ~p", [TollgateType, Id]),
              {false, AchievementItem}
          end;
       _ ->
         ?FILE_LOG_WARNING("not the tollgate", []),
         {false, AchievementItem}
      end
  end;
update_tollgate_achievement_item(star, _TollgateType, TollgateConfig, AchievementItem, AchievementConfig, EndData) ->
  TollgateID = TollgateConfig#res_stage.id,
  F = fun(Item, ItemID) -> ItemID =:= dd_util:to_integer(Item#record_item.key) end,
  Star = EndData#game_end.gain_star,
  Record =
    case cache_util:find_item_by_id(AchievementItem#achievement_item.record, F, TollgateID) of
      fail ->  [#record_item{key = dd_util:to_list(TollgateID), value = Star} | AchievementItem#achievement_item.record];
      {success, RecordItem} ->
        if
          Star =< RecordItem#record_item.value -> AchievementItem#achievement_item.record;
          true ->
            F1 = fun(Item, NItem) -> Item#record_item.key =:= NItem#record_item.key end,
            cache_util:update_list_item( AchievementItem#achievement_item.record, F1, #record_item{key = dd_util:to_list(TollgateID), value = Star})
        end
    end,
  AllStar =
    lists:foldr(
      fun(RItem, TmpNum) ->
        TmpNum + RItem#record_item.value
      end, 0, Record),
  case AchievementItem#achievement_item.finish_ts of
    0 ->
      {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = AllStar, record = Record}, AchievementConfig)};
    _ ->
      {false, AchievementItem#achievement_item{statistics = AllStar, record = Record}}
  end;

update_tollgate_achievement_item(obstacle, _TollgateType, TollgateConfig, AchievementItem, AchievementConfig, EndData) ->
  TollgateID = TollgateConfig#res_stage.id,
  F = fun(Item, ItemID) -> ItemID =:= dd_util:to_integer(Item#record_item.key) end,
  ObstacleCount = EndData#game_end.data_statistics#game_end_statistics.total_kill_obstacles,
  Record =
    case cache_util:find_item_by_id(AchievementItem#achievement_item.record, F, TollgateID) of
      fail ->  [#record_item{key = dd_util:to_list(TollgateID), value = ObstacleCount} | AchievementItem#achievement_item.record];
      {success, RecordItem} ->
         F1 = fun(Item, NItem) -> Item#record_item.key =:= NItem#record_item.key end,
         cache_util:update_list_item( AchievementItem#achievement_item.record, F1, RecordItem#record_item{value = RecordItem#record_item.value + ObstacleCount})
    end,
  AllObstacle =
    lists:foldr(
      fun(RItem, TmpNum) ->
        TmpNum + RItem#record_item.value
      end, 0, Record),
  case AchievementItem#achievement_item.finish_ts of
    0 ->
      {true, update_achievement_finish_ts(AchievementConfig#res_achievement.type_param_1, AchievementItem#achievement_item{statistics = AllObstacle, record = Record}, AchievementConfig)};
    _ ->
      {false, AchievementItem#achievement_item{statistics = AllObstacle, record = Record}}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%2014.12.26 将progress的值改为进度条显示的统计值总值，statistics为当前完成度值
update_achievement_finish_ts(80001, AchievementItem, AchievementConfig) ->       %%好友排名
  IsFinish = AchievementItem#achievement_item.statistics_1 =/= 0 andalso
    AchievementItem#achievement_item.statistics_1 =< AchievementConfig#res_achievement.condition_1,
  case IsFinish of
    true -> AchievementItem#achievement_item{finish_ts = dd_util:timestamp(), progress = 1, statistics = 1};
    false -> AchievementItem#achievement_item{progress = 1, statistics = 0}
  end;
update_achievement_finish_ts(80003, AchievementItem, AchievementConfig) ->       %%全服排名
  IsFinish = AchievementItem#achievement_item.statistics_1 =/= 0 andalso
    AchievementItem#achievement_item.statistics_1 =< AchievementConfig#res_achievement.condition_1,
  case IsFinish of
    true -> AchievementItem#achievement_item{finish_ts = dd_util:timestamp(), progress = 1, statistics = 1};
    false -> AchievementItem#achievement_item{progress = 1, statistics = 0}
  end;
update_achievement_finish_ts(_, AchievementItem, AchievementConfig) ->
  IsFinish = AchievementItem#achievement_item.statistics >= AchievementConfig#res_achievement.condition_1
    andalso AchievementItem#achievement_item.statistics_1 >= AchievementConfig#res_achievement.condition_2,
  case IsFinish of
    true -> AchievementItem#achievement_item{finish_ts = dd_util:timestamp(), progress = AchievementConfig#res_achievement.condition_1};
    false -> AchievementItem#achievement_item{progress = AchievementConfig#res_achievement.condition_1}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mission_log_flow(Uin, BeforeMissionItem, AfterMissionItem) when is_integer(Uin) andalso is_record(BeforeMissionItem, mission_item) andalso is_record(AfterMissionItem, mission_item) ->
  MissionConfig = cache_csv:get_mission_config(BeforeMissionItem#mission_item.mission_id),
  BeforeProgress = BeforeMissionItem#mission_item.mission_progress_1,
  AfterProgress = AfterMissionItem#mission_item.mission_progress_1,
  case is_mission_finish(MissionConfig#res_task.type_param_1, AfterMissionItem, MissionConfig) of
    false ->
      if
        BeforeProgress =:= 0 andalso AfterProgress > 0 ->
          cache_log_util:write_mission_log(Uin, MissionConfig#res_task.task_type, AfterMissionItem#mission_item.mission_id, MissionConfig#res_task.task_desc, 1);
        true -> ok
      end;
    true ->
      if
        BeforeProgress =:= 0 ->
          cache_log_util:write_mission_log(Uin, MissionConfig#res_task.task_type, AfterMissionItem#mission_item.mission_id, MissionConfig#res_task.task_desc, 1),
          cache_log_util:write_mission_log(Uin, MissionConfig#res_task.task_type, AfterMissionItem#mission_item.mission_id, MissionConfig#res_task.task_desc, 2);
        BeforeProgress =/= AfterProgress ->
          cache_log_util:write_mission_log(Uin, MissionConfig#res_task.task_type, AfterMissionItem#mission_item.mission_id, MissionConfig#res_task.task_desc, 2);
        true ->
          ok
      end
  end.

achievement_log_flow(false, _, _, _) -> ok;
achievement_log_flow(true, Uin, BeforeAchievementItem, AfterAchievementItem) when is_record(BeforeAchievementItem, achievement_item) andalso is_record(AfterAchievementItem, achievement_item) ->
  AchievementConfig = cache_csv:get_achievement_config(BeforeAchievementItem#achievement_item.id),
  BeforeProgress = BeforeAchievementItem#achievement_item.progress,
  AfterProgress =  AfterAchievementItem#achievement_item.progress,
  if
    BeforeProgress =:= 0 andalso AfterProgress =:= 100 ->
      cache_log_util:write_achievement_log(Uin, AchievementConfig#res_achievement.type_param_1, BeforeAchievementItem#achievement_item.id, AchievementConfig#res_achievement.desc, 1),
      cache_log_util:write_achievement_log(Uin, AchievementConfig#res_achievement.type_param_1, BeforeAchievementItem#achievement_item.id, AchievementConfig#res_achievement.desc, 2);
    BeforeProgress =:= 0 andalso AfterProgress < 100 ->
      cache_log_util:write_achievement_log(Uin, AchievementConfig#res_achievement.type_param_1, BeforeAchievementItem#achievement_item.id, AchievementConfig#res_achievement.desc, 1);
    BeforeProgress > 0 andalso AfterProgress =:= 100 ->
      cache_log_util:write_achievement_log(Uin, AchievementConfig#res_achievement.type_param_1, BeforeAchievementItem#achievement_item.id, AchievementConfig#res_achievement.desc, 2);
    true ->
      ok
  end.





