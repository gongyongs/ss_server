%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 七月 2014 上午10:05
%%%-------------------------------------------------------------------
-module(cache_work_proc).
-author("zqlt").

-include("../../deps/file_log/include/file_log.hrl").
-include("cache_def.hrl").
-include("../mail/mail.hrl").
-include("../csv.hrl").
-include("../ranking/ranking.hrl").
-include("../dd_ms.hrl").
-include("cache_cdkey.hrl").

-define(INIT_BLOCK_TIMES, 3).
-define(MAX_SEED, 2543623171).
-define(MAX_TOLLGATE, 90).
%%更新队伍宏定义
-define(MAX_TEAM_MEMBER,3).

-define(TS_RM_INIT,5).
-define(TS_RM_CD,8*60*60).

%% API
-export([execute/2]).
-export([
  update_equipment_exp/1
]).
-export([
  get_account/1,
  del_account/1,
  update_account/2
]).

-export([
  update_account_to_time/1,
  get_charge_order/2,
  add_charge_order/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% admin 修改数据   ,更新账户
execute(admin_proc, {Uin, {Action, Param}}) ->
  {success, Account} = get_account_without_create(Uin),
  {success, NAccount,FiledList}= cache_admin_work_proc:execute(Action,Uin,Param,Account),
  update_account(NAccount,FiledList);

execute(admin_proc, {Uin, {Action, Param,Oper}}) ->
  {success, Account} = get_account_without_create(Uin),
  case cache_admin_work_proc:execute(Action,Uin,Param,Account,Oper) of
    {success, NAccount,FiledList} -> update_account(NAccount,FiledList);
    {fail,Reason}->{fail,Reason}
  end;

execute(admin_del, {Uin, {Action, Param}}) ->
  {success, Account} = get_account_without_create(Uin),
  case cache_admin_work_proc:execute(Action,Uin,Param,Account) of
    success -> success;
    {fail,Reason} -> {fail,Reason}
  end;
% admin 查询数据
execute(admin_get, {Uin, {Action,Param_type}}) ->
  {success, Account} = get_account_without_create(Uin),
  cache_admin_work_proc:execute(Action,Uin,Param_type,Account);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%平台相关操作统一处理
execute(platform_oper, Operation) ->
  cache_platform:execute(Operation);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute(login, {Uin, {Ip, Device, PlatType, PlatUName, PlatDisName}}) when is_integer(Uin) ->
  {success, {Account, IsCreate}} = get_login_account(Uin),
  {NAccount, UpdateList}  = update_account_to_time(Account),
  {NNAccount, FieldList} = update_equipment_exp(NAccount),
  {NNAccount1, FieldList1} = update_inscription(NNAccount),
  {NNAAccount, NFieldList} = update_login(NNAccount1),
  NUpdateList = lists:umerge3(UpdateList, FieldList, lists:merge(NFieldList, FieldList1)),
  if
    length(NUpdateList) > 0 ->
      success = update_account(NNAAccount, NUpdateList);
    true -> ok
  end,

  %%写日志
  case IsCreate of
    false ->
      cache_log_util:write_login_log(Account#account.uin, PlatType, Device, Ip);
    true ->
      deliver_platform_continuous_login_reward(Uin, true), %%新注册用户也发放奖励
      cache_log_util:write_register_log(Account#account.uin, PlatType, PlatUName, PlatDisName, Device, Ip)
  end,

  {success, cache_work_util:encode_login_data(NNAAccount)};

execute(get_user_info, {Uin, TypeList}) when is_integer(Uin) andalso is_list(TypeList) ->
  {success, Account} = get_account(Uin),
  {NAccount, UpdateList}  = update_account_to_time(Account),
  if
    length(UpdateList) > 0 ->
      success = update_account(NAccount, UpdateList);
    true -> ok
  end,
  ValueList =
    lists:map(
      fun(Type) ->
        Value =
          case Type of
            "team" -> NAccount#account.players;
            "player" -> cache_work_util:get_player_basic_data(NAccount);
%%             "tower" -> NAccount#account.heros;
%%             "tollgate" -> NAccount#account.stage;
            "backpack" -> NAccount#account.backpack;
			"rewar_dmatch" -> NAccount#account.reward_match;
			"rewar_dmatch_ts" -> NAccount#account.reward_match_ts;
%%             "mission" -> NAccount#account.mission;
%%             "achievement" -> NAccount#account.achievement;
%%             "shop" -> NAccount#account.shop;
%%             "shop_discount" ->  get_shop_discount();
%%             "guide_step" -> NAccount#account.addition#addition.newer_guide_steps;
%%             "gift_close" -> NAccount#account.strength#strength.close_strength_gift;
%%             "rank_reward_show" -> NAccount#account.addition#addition.rank_reward_got;
%%             "strength_buy_times" -> NAccount#account.strength#strength.today_buy_times;
%%             "step_guide" -> NAccount#account.addition#addition.guide_steps;
%%             "lottery" -> get_lottery_config(NAccount#account.lottery#lottery.single_lottery_times);        %获取下次抽奖信息列表
            _ ->
              ?FILE_LOG_DEBUG("unknow type [~p]", [Type]),
              throw({custom, "HintRequestDataError"})
          end,
        {Type, Value}
      end, TypeList),
  NValueList =
    case proplists:get_value("player", TypeList, undefined) of
      undefined ->
        [{"player", cache_work_util:get_player_basic_data(NAccount)} | ValueList];
      _ -> ValueList
    end,
  {success, cache_work_util:encode_user_info(NValueList)};

execute(sync_guide_step, {Uin, GuideStep, GuideVal}) ->
  {success, Account} = get_account(Uin),
  {NAccount, UpdateList}  = update_account_to_time(Account),
  NStepList =
    case cache_util:find_guide_step_by_id(NAccount#account.addition#addition.guide_steps, GuideStep) of
      {success, _Item} ->
        cache_util:update_guide_step(NAccount#account.addition#addition.guide_steps, #record_item{key = GuideStep, value = GuideVal});
      fail ->
        [#record_item{key = GuideStep, value = GuideVal} | NAccount#account.addition#addition.guide_steps]
    end,
  NAddition = NAccount#account.addition#addition{guide_steps = NStepList},
  NNAccount = NAccount#account{addition = NAddition},
  success = update_account(NNAccount, lists:umerge3(UpdateList, ["addition"], [])),
  success;

execute(sync_user_info, {Uin, DataList}) ->
  {success, Account} = get_account(Uin),
  {NAccount, UpdateList}  = update_account_to_time(Account),
  {NNAccount, FieldList} =
    lists:foldr(
      fun({Type, Val}, {TmpAccount, TmpList}) ->
        case Type of
          "guide_step" ->
            Value = dd_util:to_integer(Val),
            if
              Value < 0 orelse Value > 20000 -> throw({custom, "HintSystemDataError"});
              true -> ok
            end,
            NAddition = TmpAccount#account.addition#addition{newer_guide_steps = dd_util:to_integer(Val)},
            NTmpAccount = TmpAccount#account{addition = NAddition},
            {NTmpAccount, ["addition" | TmpList]};
          "gift_close" ->
            Value = dd_util:to_integer(Val),
            if
              Value >= 0 andalso  Value =< 1 ->
                Strength = TmpAccount#account.strength#strength{close_strength_gift = dd_util:to_integer(Val)},
                NTmpAccount = TmpAccount#account{strength = Strength},
                {NTmpAccount, ["strength" | TmpList]};
              true ->
                throw({custom, "HintSystemDataError"})
            end;
          "rank_reward_show" ->
            Value = dd_util:to_integer(Val),
            if
              Value >=0 andalso Value =< 1 ->
                Addition = TmpAccount#account.addition#addition{rank_reward_got = Value},
                NTmpAccount = TmpAccount#account{addition = Addition},
                {NTmpAccount, ["addition" | TmpList]};
              true ->
                throw({custom, "HintSystemDataError"})
            end;
          _ ->
            ?FILE_LOG_DEBUG("unknow type [~p]", [Type]),
            throw({custom, "HintSystemDataError"})
        end
      end, {NAccount, []}, DataList),
  success = update_account_immediately(NNAccount, lists:umerge3(UpdateList, FieldList, [])),
  success;

%%检查数据：关卡是否存在，体力是否够用，道具是否存在且数量充足
%%成功需要检查掉落是否合理，得分和星级是否合理
execute(game_end, {Uin, GameEndData}) when is_integer(Uin) andalso is_record(GameEndData, game_end) ->
  {success, OAccount} = get_account(Uin),
  %%更新体力和关卡等按时间回复的数据
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查数据是否合理
  check_game_end_data_valid(Account, GameEndData),
  %%检查关卡及前置关卡
  check_tollgate(GameEndData#game_end.tollgate_id, Account#account.stage),
  %%检查体力值
  check_strength(GameEndData#game_end.tollgate_id, Account#account.stage, Account#account.strength#strength.strength),
  %%检查道具
  check_props(GameEndData#game_end.use_props, Account#account.backpack),
  TollgateConfig = cache_csv:get_tollgate_config(GameEndData#game_end.tollgate_id),
  cache_log_util:write_game_end_log(Uin, GameEndData),
  {success, NAccount, FieldList, RetV} = execute_game_end(TollgateConfig#res_stage.tollgate_type, Account, GameEndData),
  TollgateGameEndType = get_tollgate_game_end_type(GameEndData#game_end.tollgate_id),
  %%更新数据
  success = update_account(NAccount, lists:umerge3(TFieldList, FieldList, [])),
  {success, cache_work_util:encode_game_end(TollgateGameEndType, NAccount, RetV)};

%%铭文替换
execute(replace_inscription, {Uin, TowerID, InscriptionID, ReplaceID, Pos}) when is_integer(Uin) andalso is_list(TowerID) andalso is_list(InscriptionID) andalso is_list(ReplaceID) andalso is_integer(Pos) ->
  {success, Account} = get_account(Uin),
  %%更新体力和关卡等按时间回复的数据
  {NTAccount, TFieldList} = update_account_to_time(Account),

  %%检查防御塔待替换铭文是否存在
  check_tower_inscription(NTAccount#account.heros#heros.character_lists, TowerID, InscriptionID, ReplaceID, Pos),
  %%检查准备替换的铭文是否在背包中存在并检查
  check_backpack_inscription(ReplaceID, NTAccount#account.backpack#backpack.inscription_list),
  %%替换铭文，更新数据库，返回替换塔的数据以及背包的所有数据
  {NAccount, FieldList, NTower} = replace_inscription(NTAccount, TowerID, InscriptionID, ReplaceID, Pos),

  NFieldList = lists:umerge3(TFieldList, FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_replace_inscription(NAccount, NTower)};

execute(compose_inscription, {Uin, TargetInscription}) when is_integer(Uin) andalso is_list(TargetInscription) ->
  {success, Account} = get_account(Uin),
  %%更新体力和关卡等按时间回复的数据
  {NTAccount, TFieldList} = update_account_to_time(Account),

  %%检查铭文是否存在，检查铭文碎片是否存在以及数量
  check_inscription_compose(NTAccount, TargetInscription),

  %%合成铭文，更新背包，更新
  {NAccount, FieldList} = compose_inscription(NTAccount, TargetInscription),

  NFieldList = lists:umerge3(TFieldList, FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_compose_inscription(NAccount)};

execute(advance_inscription, {Uin, TowerID, InscriptionID}) ->
  {success, Account} = get_account(Uin),
  %%更新体力和关卡等按时间回复的数据
  {NTAccount, TFieldList} = update_account_to_time(Account),

  %%检查铭文是否存在，检查铭文碎片是否存在以及数量
  check_inscription_advance(NTAccount, TowerID, InscriptionID),

  %%合成铭文，更新背包，更新
  {NAccount, FieldList, NTower} = advance_inscription(NTAccount, TowerID, InscriptionID),

  NFieldList = lists:umerge3(TFieldList, FieldList, []),
  success = update_account(NAccount, NFieldList),

  {success, cache_work_util:encode_advance_inscription(NAccount, NTower)};

execute(update_tower_team, {Uin, TowerTeam}) ->
  {success, Account} = get_account(Uin),
  %%更新体力和关卡等按时间回复的数据
  {NTAccount, TFieldList} = update_account_to_time(Account),

  %%检查塔是否存在，检查更换的位置是否正确
  check_update_tower_team(NTAccount, TowerTeam),

  %%替换塔
  {NAccount, FieldList} = update_tower_team(NTAccount, TowerTeam),

  NFieldList = lists:umerge3(TFieldList, FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_update_tower_team(NAccount)};

execute(tollgate_sweep, {Uin, TollgateID, SweepTimes}) ->
  {success, OAccount} = get_account(Uin),
  %%更新体力和关卡等按时间回复的数据
  {Account, TFieldList} = update_account_to_time(OAccount),

  %%检查扫荡次数合理以及扫荡消耗体力
  check_sweep_invalid(Account, TollgateID, SweepTimes),

  %%
  {success, NAccount, FieldList, RetV} = tollgate_sweep(Account, TollgateID, SweepTimes),
  NFieldList = lists:umerge3(TFieldList, FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_tollgate_sweep(NAccount, RetV)};

%% execute(sell_material, {Uin, {SellEquipList, SellMaterialList}}) when is_integer(Uin) andalso is_list(SellEquipList) andalso is_list(SellMaterialList) ->
%%   {success, Account} = get_account(Uin),
%%
%%   %%检查装备等数据
%%   check_sell_materials(Account#account.backpack, SellEquipList, SellMaterialList),
%%   %%更新数据
%%   {GainGold, NBackpack} = update_backpack_with_sell_materials(Account#account.backpack, SellEquipList, SellMaterialList),
%%   NAccount = cache_api:account_inc_money(Account, 2, GainGold, "SellMaterial"),
%%   NNAccount = NAccount#account{backpack = NBackpack},
%%
%%   FieldList =  ["backpack", "gold_coin"],
%%
%%   %%更新体力和关卡等按时间回复的数据
%%   {NTAccount, TFieldList} = update_account_to_time(NNAccount),
%%   NFieldList = lists:umerge3(TFieldList, FieldList, []),
%%
%%   success = update_account(NTAccount,NFieldList),
%%   {success, NTAccount};

%%%%%%%%%%%测试接口%%%
%% execute(add_material, {Uin, {AddList, AddGold, AddGem, AddStrength}}) when is_integer(Uin) andalso is_list(AddList) andalso is_integer(AddGold) andalso is_integer(AddGem) andalso is_integer(AddStrength) ->
%%   {success, Account} = get_account(Uin),
%%
%%   %%检查数据
%%   NBackpack = test_add_equipment_material(Uin, Account#account.backpack, AddList),
%%   NGold = Account#account.gold_coin + AddGold,
%%   NGem = Account#account.gem + AddGem,
%%   NStrength =  Account#account.strength#strength{strength = Account#account.strength#strength.strength + AddStrength},
%%
%%   NAccount = Account#account{backpack = NBackpack, gold_coin = NGold, gem = NGem, strength = NStrength},
%%
%%   FieldList = ["backpack", "gold_coin", "gem", "strength"],
%%   %%更新体力和关卡等按时间回复的数据
%%   {NTAccount, TFieldList} = update_account_to_time(NAccount),
%%   NFieldList = lists:umerge3(TFieldList, FieldList, []),
%%
%%   success = update_account(NTAccount, NFieldList),
%%   {success, NTAccount};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute(click_world_map_block, {Uin, BlockID, MaterialList}) when is_integer(Uin) andalso is_list(MaterialList) ->
  {success, OAccount} = get_account(Uin),
  %%更新体力和关卡等按时间回复的数据
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查WBlockID是否存在及是否已经解锁及检查今天是否还有点击的机会,检查掉落的材料是否合格
  check_world_map_block(Account#account.stage, BlockID, MaterialList),
  %%更新点击次数及点击时间，更新
  {HarvestList, UpdateHarvestItem} =
    case cache_util:find_world_map_block(Account#account.stage#stage.harvest_obstacles_list, BlockID) of
      {success, WBItem} ->
        Times = WBItem#harvest_obstacles_item.remain_number - 1,
        NItem = WBItem#harvest_obstacles_item{remain_number = Times, last_update_ts = dd_util:timestamp()},
        {cache_util:update_harvest_obstacle(Account#account.stage#stage.harvest_obstacles_list, NItem), NItem};
      fail ->
        NItem = #harvest_obstacles_item{id = BlockID, remain_number = ?INIT_BLOCK_TIMES - 1, last_update_ts = dd_util:timestamp()},
        {[NItem | Account#account.stage#stage.harvest_obstacles_list], NItem}
    end,
  NStage = Account#account.stage#stage{harvest_obstacles_list = HarvestList},
  %%更新背包数据
  NBackpack =
    lists:foldr(
      fun({ID, Count}, TempNB) ->
        DropConfig = cache_csv:get_drop_config(ID),
        update_backpack_by_drop(Uin, DropConfig, TempNB, Count)
      end, Account#account.backpack, MaterialList),

  %%更新相关成就
  {UpdateAchievementGroupList, Achievement} = cache_mission:update_achievement(harvest_obstacle, Account#account.uin, Account#account.achievement, BlockID),
  NAccount = Account#account{backpack = NBackpack, stage = NStage, achievement = Achievement},
  FieldList = ["backpack", "stage", "achievement"],
  NFieldList = lists:umerge3(TFieldList, FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_world_map_block(NAccount, UpdateHarvestItem, UpdateAchievementGroupList)};

execute(strengthen, {Uin, {Type, ObjectType, ObjectID, TargetID, TowerID, PieceList}}) when is_integer(Uin) andalso is_list(PieceList) andalso is_integer(Type) andalso is_integer(ObjectType) andalso is_list(ObjectID) andalso is_list(TargetID) andalso is_list(TowerID)->
  {success, OAccount} = get_account(Uin),
  %%更新体力和关卡等按时间回复的数据
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查强化数据
  check_strengthen_data(Account, Type, ObjectType, ObjectID, TargetID, TowerID, PieceList),
  {NAccount, FieldList, {StrengthenItem, StrengthenData}} = strengthen(Account, Type, ObjectType, ObjectID, TargetID, TowerID, PieceList),

  %%更新相关成就
  {UpdateAchievementGroupList, NAchievement} = cache_mission:update_achievement(strengthen, Uin, NAccount#account.achievement, StrengthenData),
  {UpdateMissionList, NMission} = cache_mission:update_mission(strengthen, Uin, NAccount#account.mission, StrengthenData),

  NFieldList = lists:umerge3(TFieldList, FieldList, ["achievement", "mission"]),
  NNAccount = NAccount#account{achievement = NAchievement, mission = NMission},
  success = update_account(NNAccount, NFieldList),
  {success, cache_work_util:encode_equipment_strengthen(NNAccount, StrengthenItem, UpdateAchievementGroupList, UpdateMissionList)};

execute(get_login_reward, {Uin, LoginTimes}) when is_integer(Uin) andalso is_integer(LoginTimes) ->
  {success, OAccount} = get_account(Uin),
  %%更新体力和关卡等按时间回复的数据
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检差登陆奖励的合理
  check_login_reward(Account#account.login_reward, LoginTimes),
  %%领取奖励
  {NAccount, FieldList} = get_login_reward(Account, LoginTimes),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_login_reward(NAccount)};

%%商城购买
execute(buy_goods, {Uin, GoodsId, Ip}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查商品类型，检查商品是否在购买期限内，且检查商品今天已经没有购买次数了
  %%检查money是否充足
  check_commodity_valid(Account, GoodsId),
  {NAccount, FieldList, {UpdateMissionList}} = buy_goods(Account, Uin, GoodsId, Ip),
  NFieldList = lists:umerge3(TFieldList, FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_buy_goods(NAccount, UpdateMissionList)};

%%领取活跃度奖励
execute(get_activity_reward, {Uin, ActivityID}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查奖励是否已领取或者是否达到
  check_activity_reward_valid(Account, ActivityID),
  {NAccount, FieldList} = get_activity_reward(Account, ActivityID),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_activity_reward(NAccount)};

%%领取任务奖励
execute(get_mission_reward, {Uin, MissionID}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查奖励是否已领取或者是否达到
  check_mission_reward_valid(Account, MissionID),
  {NAccount, FieldList, MissionItem} = get_mission_reward(Account, MissionID),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_mission_reward(NAccount, [MissionItem])};

%%领取成就奖励
execute(get_achievement_reward, {Uin, GroupID, AchievementID}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查奖励是否已领取或者是否达到
  check_achievement_reward_valid(Account, GroupID, AchievementID),
  {NAccount, FieldList, AchievementGroup} = get_achievement_reward(Account, GroupID, AchievementID),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_achievement_reward(NAccount, [AchievementGroup])};

%%赠送体力
execute(present_friend_strength, {Uin, FriendUin}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查好友的合理性,以及该好友是否还能赠送
  check_friend_valid(Account, FriendUin),
  {NAccount, FieldList} = present_friend_strength(Account, FriendUin),

  %%更新成就和任务
  {UpdateMissionList, Mission} = cache_mission:update_mission(strength_gift, Uin, NAccount#account.mission, {1, 1}),
  {UpdateAchievementList, Achievement} = cache_mission:update_achievement(strength_gift, Uin, NAccount#account.achievement, {1, 1}),

  NewAccount = NAccount#account{achievement = Achievement, mission = Mission},

  NFieldList = lists:umerge3(TFieldList,FieldList, ["achievement", "mission"]),
  success = update_account_immediately(NewAccount, NFieldList),    %%立即更新数据库
  {success, cache_work_util:encode_present_friend_strength(NewAccount, UpdateMissionList, UpdateAchievementList)};

execute(enter_tollgate, {Uin, TollgateID}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  {NAccount, FieldList, {RandSeed, RandValue}} = get_tollgate_drop_germ(Account, TollgateID),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_enter_tollgate(NAccount, {RandSeed, RandValue})};

execute(get_endless_germ, Uin) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  {NAccount, FieldList, {_Count}} = generate_endless_drop_germ(Account),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_endless_germ(NAccount)};

execute(lottery, {Uin, LotteryType}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查钻石是否充足
  check_lottery_valid(Account, LotteryType),
  {NAccount, FieldList, {DropList}} = lottery(Account, LotteryType),

  %%更新任务和成就
  {UpdateMissionList, Mission} = cache_mission:update_mission(lottery, Uin, NAccount#account.mission, LotteryType),
  NFieldList = lists:umerge3(TFieldList,FieldList, ["mission"]),
  NNAccount = NAccount#account{mission = Mission},
  success = update_account(NNAccount, NFieldList),
  {success, cache_work_util:encode_lottery(NNAccount, DropList, get_lottery_config(NNAccount#account.lottery#lottery.single_lottery_times), UpdateMissionList)};


execute(iap_buy, {Uin, Orders, ProductID, Receipt}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  ProductInfo =
    try
      [PI] =  get_commodity_from_config_by_product_id(ProductID),
      PI
    catch
      What:Type ->
        cache_log_util:write_fail_pay_log(Account#account.uin, "IOS", Orders, Receipt, "ProductIDNotExist"),
        ?FILE_LOG_ERROR("What = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
        throw({fail, "HintPurchaseError"})
    end,
  {NAccount, FieldList, {BuyCount}} =
    try
      %%检查钻石id是否存在
      check_iap_valid(Account, Orders, ProductID, ProductInfo),
      iap_buy(Account, Orders, ProductID, ProductInfo)
    catch
      throw:{custom, Reason} ->
        cache_log_util:write_fail_pay_log(Account#account.uin, "IOS", Orders,  Receipt, Reason),
        ?FILE_LOG_DEBUG("iap buy error, [~p]", [Reason]),
        throw({custom, "HintPurchaseError"});
      What1:Type1 ->
        cache_log_util:write_fail_pay_log(Account#account.uin, "IOS", Orders, Receipt, "HandleException"),
        ?FILE_LOG_ERROR("What = ~p, type = ~p, stack = ~p", [What1, Type1, erlang:get_stacktrace()]),
        throw({fail, "HintPurchaseError"})
    end,
  %%更新成就和任务
  {UpdateMissionList, Mission} = cache_mission:update_mission(charge, Uin, NAccount#account.mission, BuyCount),
  NNAccount = NAccount#account{mission = Mission},
  NFieldList = lists:umerge3(TFieldList,FieldList, ["mission"]),
  success = update_account(NNAccount, NFieldList),

  %%输出流水日志
  cache_log_util:write_success_pay_log(Account#account.uin, "IOS", Orders, ProductID, ProductInfo#res_goods.goods_type, [], ProductInfo#res_goods.goods_count, dd_util:to_list(Orders), ProductInfo#res_goods.money_count),

  {success, cache_work_util:encode_iap_buy(NNAccount, UpdateMissionList)};

execute(get_mail_attach, {Uin, MailID}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查附件是否存在
  {NAccount, FieldList, {UpdateMissionList, UpdateAchievementList}} = get_mail_attach(Account, MailID),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_attach_mail(NAccount, UpdateMissionList, UpdateAchievementList)};

execute(share_score, {Uin, _PlatName}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  {UpdateMissionList, Mission} = cache_mission:update_mission(share, Uin, Account#account.mission, dd_util:timestamp()),
  NAccount = Account#account{mission = Mission},
  NFieldList = lists:umerge3(TFieldList, ["mission"], []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_share_score(NAccount, UpdateMissionList)};
%%settlement_friend_rank_reward/2,
%%settlement_server_rank_reward/4
execute(settlement_server_rank_reward, {Uin, Percent, Rank, Point}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),

  %%发放奖励
  {NAccount, FieldList, {Ret}} = settlement_server_rank_reward(Account, Percent, Rank, Point),
  {_UpdateAchievementGroupList, Achievement} = cache_mission:update_achievement(rank, Uin, Account#account.achievement, {-1, Percent}),
  NNAccount = NAccount#account{achievement = Achievement},
  NFieldList = lists:umerge3(TFieldList, ["achievement"], FieldList),
  success = update_account(NNAccount, NFieldList),
  Ret;
execute(settlement_friend_rank_reward, {Uin, RankItem}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%发放奖励
  Ret = settlement_friend_rank_reward(Uin, RankItem),
  {_UpdateAchievementGroupList, Achievement} = cache_mission:update_achievement(rank, Uin, Account#account.achievement, {RankItem#rank_info.rank, -1}),
  NAccount = Account#account{achievement = Achievement},
  NFieldList = lists:umerge3(TFieldList, ["achievement"], []),
  success = update_account(NAccount, NFieldList),
  Ret;
execute(fast_purchase, {Uin, PurchaseType, Value}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%发放奖励
  check_fast_purchase(Account, PurchaseType, Value),
  {NAccount, FieldList} = fast_purchase(Account, PurchaseType, Value),
  NFieldList = lists:umerge3(TFieldList, FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_fast_purchase(NAccount)};

execute(get_user_basic_info, {Uin, UinList}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  InfoList = get_user_basic_info(UinList),
  success = update_account(Account, TFieldList),
  {success, cache_work_util:encode_user_basic_info(InfoList)};

execute(get_mass_attach_mail, {Uin, Type}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  %%检查附件是否存在
  {NAccount, FieldList, {UpdateMissionList, UpdateAchievementList}} = get_mass_attach_mail(Account, Type),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_mass_attach_mail(NAccount, UpdateMissionList, UpdateAchievementList)};

execute(get_server_endless_rank, Uin) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  {success, EndlessServerRankList} = cache_ranking:get_server_endless_rank(Uin),
  success = update_account(Account, TFieldList),
  {success, cache_work_util:encode_server_endless_rank(EndlessServerRankList)};

execute(get_friend_endless_rank, Uin) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  {success, FriendRankInfo} = cache_ranking:get_friend_endless_rank(Uin,
    get_friend_uin_list(Account#account.platform_info), get_energy_gift_uin_list(Account#account.strength)),
  success = update_account(Account, TFieldList),
  {success, cache_work_util:encode_friend_endless_rank(FriendRankInfo)};

execute(op_bag, {Uin,_OP,_Params}) ->
  {success, Account} = get_account(Uin),
  {NAccount, UpdateList}  = update_account_to_time(Account),
  if
    length(UpdateList) > 0 ->
      success = update_account(NAccount, UpdateList);
    true -> ok
  end,
  {success, cache_work_util:encode_ss_backpack(NAccount#account.backpack)};

execute(op_bag_add, {Uin, GoodsId,AddCount, Ip}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  {NAccount, FieldList} = add_item_ss(Account, Uin, GoodsId,AddCount, Ip),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, ""};

execute(op_bag_del, {Uin, GoodsId,DelCount, Ip}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  {NAccount, FieldList} = del_item_ss(Account, Uin, GoodsId,DelCount, Ip),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, ""};

%%更新球员队伍
execute(change_team,{Uin,SuperTeam})->
	{success, OAccount} = get_account(Uin),
	%%更新一些按时间回复的数据
	{NTAccount, TFieldList} = update_account_to_time(OAccount),
    %%校验下队伍的基本情况
  	check_super_team(NTAccount, SuperTeam),
  	%%替换队伍列表
  	{NAccount, FieldList} = update_super_team(NTAccount, SuperTeam),
  	NFieldList = lists:umerge3(TFieldList, FieldList, []),
  	success = update_account(NAccount, NFieldList),
  	{success,cache_work_util:encode_super_team(SuperTeam)};

execute(op_rewardmatch, {Uin, OP, Params})->
	{success, OAccount} = get_account(Uin),
	{NAccount, TFieldList}  = update_account_to_time(OAccount),
	{NNAccount, FieldList,_ReturnData} = mod_rewardmatch:execute(OP,{NAccount,Params}),
	NFieldList = lists:umerge3(TFieldList,FieldList, []),
	success = update_account(NNAccount, NFieldList),
	{success, cache_work_util:encode_ss_rewardmatch(NNAccount#account.reward_match)};

execute(op_ladder, {Uin, OP, Params})->
  ?FILE_LOG_DEBUG("op_ladder OP, [~p]", [OP]),
  {success, OAccount} = get_account(Uin),
  {NAccount, TFieldList}  = update_account_to_time(OAccount),
  {NNAccount, FieldList,_ReturnData} = mod_ladder:execute(OP,{NAccount,Params}),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NNAccount, NFieldList),
  ?FILE_LOG_DEBUG("op_ladder _ReturnData, [~p]", [_ReturnData]),
  CurTime = f_t_time_pb:encode_f_t_curtime(_ReturnData),
  ?FILE_LOG_DEBUG("op_ladder CurTime, [~p]", [CurTime]),
  {success, CurTime};

execute(exchange_cdkey, {Uin, CodeID}) ->
  {success, OAccount} = get_account(Uin),
  {Account, TFieldList} = update_account_to_time(OAccount),
  {success, NAccount, FieldList, PackageList} = exchange_cdkey(Account, CodeID),
  NFieldList = lists:umerge3(TFieldList,FieldList, []),
  success = update_account(NAccount, NFieldList),
  {success, cache_work_util:encode_exchange_cdkey(NAccount, PackageList)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%内部函数接口%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%更新上阵队伍列表
check_super_team(Account,SuperTeam) when is_record(Account,account)->
	case length(SuperTeam) of
		?MAX_TEAM_MEMBER->
			ok;
		_->
			throw({custom, "HintChangeTeamDataError"})
	end,
	%%检查球员是否存在玩家身上（暂时球员背包功能么有，先注释不做校验）
%% 	Fun = fun({_,ID})->
%% 				  case cache_util:find_player_by_id(Account#account.players#players.players_lists,ID) of
%% 					  {success,_}->
%% 						  ok;
%% 					  _->
%% 						  throw({custom,"HintSuperNotExist"})
%% 				  end
%% 			   end,
%% 	lists:foreach(Fun, SuperTeam),
	ok.

%%替换最新的队伍列表
update_super_team(Account,SuperTeam)when is_record(Account,account)->
	NewPlayers = Account#account.players#players{select_players = SuperTeam},
	{Account#account{players = NewPlayers},["player"]}.

exchange_cdkey(Account, CodeID) when is_record(Account, account) andalso is_list(CodeID) ->
  case cache_cdkey:query_cdkey_by_id(CodeID) of
    fail ->
      ?FILE_LOG_ERROR("query_cdkey_by_id error, ~p", [CodeID]),
      throw({custom, "HintInvalidCDKEY"});
    not_exist ->
      ?FILE_LOG_DEBUG("cdkey ~p not exist", [CodeID]),
      throw({custom, "HintInvalidCDKEY"});
    has_exchanged ->
      ?FILE_LOG_DEBUG("cdkey ~p has exchanged", [CodeID]),
      throw({custom, "HintInvalidCDKEY"});
    invalid ->
      ?FILE_LOG_DEBUG("cdkey is invalid, maybe is expiration", [CodeID]),
      throw({custom, "HintInvalidCDKEY"});
    {success, CDKey} ->
      ?FILE_LOG_DEBUG("cdkey = ~p", [CDKey]),
      CDKeyPackageConfig = cache_csv:get_cdkey_package_config_by_id(CDKey#cdkey_rd.code_content),
      check_cdkey(Account, CDKey, CDKeyPackageConfig),
      %%针对有限制次数的激活码做校验
      ?FILE_LOG_DEBUG("get cdkey package, cdkey = ~p, package id = ~p", [CDKey, CDKeyPackageConfig#res_cdkey_package_config.id]),
      {NAccount, PackageList} = inc_cdkey_package(Account, CDKeyPackageConfig),
      success = cache_cdkey:exchange_cdkey(CodeID),
      {success, NAccount, ["gold_coin", "gem", "strength", "addition", "backpack"], PackageList}
  end.

tollgate_sweep(Account, TollgateID, SweepTimes) when is_record(Account, account) andalso is_integer(TollgateID) andalso is_integer(SweepTimes) andalso SweepTimes > 0 andalso TollgateID < 10000 ->
  %%掉落物品
  {TollgateDrop, SweepDrop} = get_tollgate_sweep_drop(Account#account.stage#stage.tollgate_drop, SweepTimes, TollgateID),
  {success, NAccount, DropItemList} = update_account_with_sweep_drop(Account, SweepDrop),
  %%刷新任务， 不需要刷新成就。
  {UpdateMissionList, NMission} = cache_mission:update_mission(sweep, NAccount#account.uin, NAccount#account.mission, {TollgateID, SweepTimes}),
  %%减少体力
  CostEnergy = get_sweep_energy_cost(TollgateID, NAccount#account.stage, SweepTimes),
  ?FILE_LOG_DEBUG("tollgate sweep times = ~p, cost energy = ~p", [SweepTimes, CostEnergy]),
  NewAccount = cache_api:dec_strength(NAccount, CostEnergy, "TollgateSweep"),
  %%更新关卡次数
  {NStage, UpdateTollgate} =
    case cache_util:find_finished_tollgate_by_id(NewAccount#account.stage, TollgateID) of
      {success, Tollgate} ->
        NTollgate = update_tollgate_remain_times(sweep, Tollgate, SweepTimes),
        {cache_util:update_tollgate(NewAccount#account.stage, NTollgate), NTollgate};
      fail -> throw({custom, "HintCannotSweep"})
    end,
  NNStage = NStage#stage{tollgate_drop = TollgateDrop},
  NewAccount1 = NewAccount#account{mission = NMission, stage = NNStage},
  {success, NewAccount1, ["gold_coin", "gem", "stage", "mission", "backpack", "strength"], {UpdateMissionList, UpdateTollgate, DropItemList}}.



%%扫荡 更新掉落
update_account_with_sweep_drop(Account, SweepDropList) when is_record(Account, account) andalso is_list(SweepDropList) ->
  %%更新掉落
  DropItemList =
    lists:map(
      fun(DropsList) ->
        lists:foldl(
          fun({ID, Count}, TmpL) ->
            List = lists:foldl(
              fun(_Index, TmpItemList) ->
                DropNConfig = cache_csv:get_tollgate_drop_by_id(ID),
                ChestConfig = cache_csv:get_treasure_config_by_id(DropNConfig#res_tollgate_drop.treasure_id),
                {TreasureItemList, TotalProb} = get_treasure_item_list(Account, TmpItemList, ChestConfig#res_treasure_config.item_list),
                if
                  TotalProb =< 0 -> throw({custom, "HintSystemDataError"});
                  true -> ok
                end,
                RandValue = dd_util:random_in_range(?MAX_SEED) rem TotalProb,
                {success, Item} = compare_treasure_prob(TreasureItemList, RandValue),
                DropItem = #treasure_item{drop_id = Item#res_treasure_item_config.item_id, sub_id = Item#res_treasure_item_config.sub_drop_id,
                treasure_id = ID, drop_type = Item#res_treasure_item_config.item_type, count = Item#res_treasure_item_config.item_count, drop_level = Item#res_treasure_item_config.item_level},
                [DropItem | TmpItemList]
              end, [], lists:seq(1, Count)),
            lists:merge(TmpL, List)
          end, [], DropsList)
      end, SweepDropList),
  ?FILE_LOG_DEBUG("sweep drop treasure list = ~p", [DropItemList]),
  NAccount = update_account_with_treasure_list(lists:flatten(DropItemList), <<"TollgateDrop">>, Account),
  {success, NAccount, DropItemList}.


%%生成掉落
get_tollgate_sweep_drop(TollgateDrop, SweepTimes, TollgateID) when is_record(TollgateDrop, tollgate_drop) andalso is_integer(SweepTimes) andalso is_integer(TollgateID) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  TollgateDropList =
    [
      {TollgateConfig#res_stage.drop_1, TollgateConfig#res_stage.drop_1_prob},
      {TollgateConfig#res_stage.drop_2, TollgateConfig#res_stage.drop_2_prob},
      {TollgateConfig#res_stage.drop_3, TollgateConfig#res_stage.drop_3_prob}
    ],
  RandV = dd_util:get_rand_val(TollgateDrop#tollgate_drop.rand_val),
  Seed = dd_util:random_in_range(?MAX_SEED) + ?MAX_SEED,
  {DropList, {FinalRandV, FinalSeed}} =
    lists:foldl(
      fun(_, {TmpList, {TmpRand, TmpSeed}}) ->
        L = [gen_tollgate_drop(TollgateDropList, TmpSeed, TmpRand) | TmpList],
        TV = dd_util:get_rand_val(TollgateDrop#tollgate_drop.rand_val),
        TS = dd_util:random_in_range(?MAX_SEED) + ?MAX_SEED,
        {L, {TV, TS}}
      end, {[],{RandV, Seed}}, lists:seq(1, SweepTimes)),
  ?FILE_LOG_DEBUG("sweep ~p, drop is = ~p", [SweepTimes, DropList]),
  {TollgateDrop#tollgate_drop{seed_val = FinalSeed, rand_val = FinalRandV, seed_ts = dd_util:timestamp()}, DropList}.

check_sweep_invalid(Account, TollgateID, SweepTimes) when is_record(Account, account) andalso is_integer(TollgateID) andalso is_integer(SweepTimes) andalso SweepTimes > 0 andalso TollgateID < 10000 ->
  %%检查是否满足三星条件
  %%检查扫荡次数合理
  case cache_util:find_finished_tollgate_by_id(Account#account.stage, TollgateID) of
    {success, Tollgate} ->
      if
        Tollgate#tollgate.max_star >= 3 -> ok;
        true -> throw({custom, "HintCannotSweep"})
      end,
      DailyRemain = Tollgate#tollgate.daily_remain_times,
      DailyAdditionRemain = Tollgate#tollgate.addition_remain_times,
      if
        DailyRemain > 0 ->
          if
            SweepTimes > DailyRemain -> throw({custom, "InvalidSweepData"});
            true -> ok
          end;
        DailyAdditionRemain > 0 ->
          if
            SweepTimes > DailyAdditionRemain -> throw({custom, "InvalidSweepData"});
            true -> ok
          end;
        true -> throw({custom, "HintTollgateNoTimes"})
      end;
    fail -> throw({custom, "HintCannotSweep"})
  end,
  %%检查体力是否充足
  CostEnergy = get_sweep_energy_cost(TollgateID, Account#account.stage, SweepTimes),
  if
    CostEnergy > Account#account.strength#strength.strength -> throw({custom, "HintSufficientEnergy"});
    true -> ok
  end.

%%检查塔是否存在，检查更换的位置是否正确
check_update_tower_team(Account, TowerTeam) when is_record(Account, account) andalso is_list(TowerTeam) ->
  %%确定装备五个塔
  case length(TowerTeam) of
    5 -> ok;
    _ -> throw({custom, "HintTowerDataError"})
  end,
  %%检查替换的塔是否存在
  lists:foreach(
    fun({_, ID}) ->
      case cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, ID) of
        {success, _} -> ok;
        _ -> throw({custom, "HintTowerNotExist"})
      end
    end, TowerTeam).

%%替换塔
update_tower_team(Account, TowerTeam) when is_record(Account, account) andalso is_list(TowerTeam) ->
  %%更新塔
  NHeros = Account#account.heros#heros{select_hero = TowerTeam},
  ?FILE_LOG_DEBUG("update tower team = ~p", [TowerTeam]),
  {Account#account{heros = NHeros}, ["hero"]}.

%%检查铭文是否存在，检查铭文碎片是否存在以及数量
check_inscription_advance(Account, TowerID, InscriptionID) when is_record(Account, account) andalso is_list(TowerID) andalso is_list(InscriptionID) ->
  {success, TowerItem} =
    case TowerID of
      "" ->
        ?FILE_LOG_ERROR("check_inscription_advance => tower id not is null", []),
        throw({custom, "HintSystemError"});
      _ ->
        case cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, TowerID) of
          {success, Tower} -> {success, Tower};
          fail ->
            ?FILE_LOG_ERROR("check_inscription_advance => tower not exist", []),
            throw({custom, "HintSystemError"})
        end
    end,
  case cache_util:find_inscription_by_id(TowerItem#character.inscription_list, InscriptionID) of
    {success, _Inscription}  ->
      InscriptionConfig = cache_csv:get_inscription_by_id(InscriptionID),
      AdvanceID = InscriptionConfig#res_inscription.evolve_id,
      AdvanceInscriptionConfig = cache_csv:get_inscription_by_id(AdvanceID),
      AdvanceNeedPiece = AdvanceInscriptionConfig#res_inscription.evol_need_piece,
      AdvanceNeedPieceCount = AdvanceInscriptionConfig#res_inscription.evol_need_count,
      %%检查数量
      case gb_trees:lookup(AdvanceNeedPiece, Account#account.backpack#backpack.inscription_piece_list) of
        none ->
          ?FILE_LOG_ERROR("check_inscription_advance => piece not exist, ~p", [AdvanceNeedPiece]),
          throw({custom, "HintSystemError"});
        {value, Cnt} ->
          if
            Cnt < AdvanceNeedPieceCount ->
              ?FILE_LOG_ERROR("check_inscription_advance => piece count not enough, ~p", [AdvanceNeedPieceCount]),
              throw({custom, "HintInsufficentPiece"});
            true -> ok
          end
      end;
    fail ->
      ?FILE_LOG_ERROR("check_inscription_advance => tower do not have, tower = ~p, inscription = ~p", [TowerItem, InscriptionID]),
      throw({custom, "HintSystemError"})
  end.

%%合成铭文，更新背包，更新
advance_inscription(Account, TowerID, InscriptionID) when is_record(Account, account) andalso is_list(TowerID) andalso is_list(InscriptionID) ->
  {success, TowerItem} =
    case cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, TowerID) of
      {success, Tower} -> {success, Tower};
      fail ->
        ?FILE_LOG_ERROR("advance_inscription => tower not exist", []),
        throw({custom, "HintSystemDataError"})
    end,
  {success, Inscription} = cache_util:find_inscription_by_id(TowerItem#character.inscription_list, InscriptionID),
  InscriptionConfig = cache_csv:get_inscription_by_id(InscriptionID),
  AdvanceID = InscriptionConfig#res_inscription.evolve_id,
  AdvanceInscriptionConfig = cache_csv:get_inscription_by_id(AdvanceID),
  AdvanceNeedPiece = AdvanceInscriptionConfig#res_inscription.evol_need_piece,
  AdvanceNeedPieceCount = AdvanceInscriptionConfig#res_inscription.evol_need_count,
  %%检查数量
  case gb_trees:lookup(AdvanceNeedPiece, Account#account.backpack#backpack.inscription_piece_list) of
    none ->
      ?FILE_LOG_ERROR("advance_inscription => piece not exist, ~p", [AdvanceNeedPiece]),
      throw({custom, "HintSystemError"});
    {value, Cnt} ->
      %%更新碎片数量
      PieceTree = gb_trees:update(AdvanceNeedPiece, Cnt - AdvanceNeedPieceCount, Account#account.backpack#backpack.inscription_piece_list),
      NInscription = Inscription#inscription{id = AdvanceID},
      NInscriptionList = cache_util:update_inscription_by_pos(TowerItem#character.inscription_list, NInscription),
      NTowerItem = TowerItem#character{inscription_list = NInscriptionList},
      HeroList = cache_util:update_tower(Account#account.heros#heros.character_lists, NTowerItem),
      NHero = Account#account.heros#heros{character_lists = HeroList},
      NBackpack = Account#account.backpack#backpack{inscription_piece_list = PieceTree},
      NAccount = Account#account{backpack = NBackpack, heros = NHero},
      {NAccount, ["backpack", "hero"], NTowerItem}
  end.

%%检查铭文是否存在，检查铭文碎片是否存在以及数量
check_inscription_compose(Account, TargetInscription) ->
  Tree = get_user_inscription_class(Account),
  InscriptionConfig = cache_csv:get_inscription_by_id(TargetInscription),
  case gb_trees:lookup(InscriptionConfig#res_inscription.inscription_class, Tree) of
    none -> ok;
    {value, _} ->
      ?FILE_LOG_ERROR("composed inscription has exist, invalid compose operation, ~p", [TargetInscription]),
      throw({custom, "HintInscriptionExist"})
  end,
  case InscriptionConfig#res_inscription.compose_piece of
    "" ->
      ?FILE_LOG_ERROR("inscrition can not be composed, ~p", [TargetInscription]),
      throw({custom, "HintInscriptionCantCompose"});
    _ -> ok
  end,
  case gb_trees:lookup(InscriptionConfig#res_inscription.compose_piece, Account#account.backpack#backpack.inscription_piece_list) of
    none ->
      ?FILE_LOG_ERROR("compose inscription piece not exist", []),
      throw({custom, "HintInsufficentPiece"});
    {value, Count} ->
      if
        Count >= InscriptionConfig#res_inscription.compose_count -> ok;
        true ->
          ?FILE_LOG_ERROR("compose inscrition piece not enough", []),
          throw({custom, "HintInsufficentPiece"})
      end
  end.

%%合成铭文，更新背包，更新
compose_inscription(Account, TargetInscription) ->
  InscriptionConfig = cache_csv:get_inscription_by_id(TargetInscription),
  CostPieceCount = InscriptionConfig#res_inscription.compose_count,
  PieceID = InscriptionConfig#res_inscription.compose_piece,
  %%消耗碎片
  BackpackPieceList = cache_api:dec_tree_element([{PieceID, CostPieceCount}], Account#account.backpack#backpack.inscription_piece_list),

  %%增加铭文
  NBackpack = Account#account.backpack#backpack{inscription_piece_list = BackpackPieceList, inscription_list = [#inscription{id = TargetInscription, pos = InscriptionConfig#res_inscription.type} | Account#account.backpack#backpack.inscription_list]},

  {Account#account{backpack = NBackpack}, ["backpack"]}.


get_user_inscription_class(Account) ->
  Tree =
    lists:foldl(
      fun(Inscription, TmpTree) ->
        InscriptionConfig = cache_csv:get_inscription_by_id(Inscription#inscription.id),
        case gb_trees:lookup(InscriptionConfig#res_inscription.inscription_class, TmpTree) of
          none ->
            gb_trees:insert(InscriptionConfig#res_inscription.inscription_class, 1, TmpTree);
          {value, _} -> TmpTree
        end
      end, gb_trees:empty(), Account#account.backpack#backpack.inscription_list),
  lists:foldl(
    fun(Tower, TmpT) ->
      lists:foldl(
        fun(Ins, TmpTTree) ->
          InsConfig = cache_csv:get_inscription_by_id(Ins#inscription.id),
          case gb_trees:lookup(InsConfig#res_inscription.inscription_class, TmpTTree) of
            none ->
              gb_trees:insert(InsConfig#res_inscription.inscription_class, 1, TmpTTree);
            {value, _} -> TmpTTree
          end
        end, TmpT, Tower#character.inscription_list)
    end, Tree, Account#account.heros#heros.character_lists).

%%检查要替换的铭文的类型是否匹配
check_tower_inscription(HeroList, TowerID, InscriptionID, ReplaceID, InscriptionPos) when is_list(HeroList) andalso is_list(TowerID) ->
  {success, TowerItem} = cache_util:find_tower_by_id(HeroList, TowerID),
  if
    ReplaceID =:= "" andalso InscriptionID =:= "" -> throw({custom, "HintRequestDataError"});
    InscriptionPos > 4 -> throw({custom, "HintRequestDataError"});
    true -> ok
  end,
  case cache_util:find_inscription_by_pos(TowerItem#character.inscription_list, InscriptionPos) of
    {success, Ins} ->
      if
        InscriptionID =:= Ins#inscription.id -> ok;
        true ->
          ?FILE_LOG_ERROR("inscription id in the pos is not right, ~p, ~p, ~p", [InscriptionID, InscriptionPos, Ins]),
          throw({custom, "HintRequestDataError"})
      end;
    fail ->
      if
        InscriptionID =:= "" -> ok;
        true ->
          ?FILE_LOG_ERROR("inscription id in the pos is not right, ~p, ~p", [InscriptionID, InscriptionPos]),
          throw({custom, "HintRequestDataError"})
      end
  end,
  case ReplaceID of
    "" ->  %%卸载
      if
        InscriptionID =:= "" -> throw({custom, "HintRequestDataError"});
        true -> ok
      end;
    _ ->
      ReplaceInscriptionConfig = cache_csv:get_inscription_by_id(ReplaceID),
      case InscriptionID of
        "" ->
          if
            InscriptionPos =:= ReplaceInscriptionConfig#res_inscription.type -> ok;
            true ->
              ?FILE_LOG_ERROR("inscription type error, ~p, ~p", [InscriptionPos, ReplaceInscriptionConfig]),
              throw({custom, "HintRequestDataError"})
          end;
        _ ->
          InscriptionConfig = cache_csv:get_inscription_by_id(InscriptionID),
          if
            InscriptionPos =:= InscriptionConfig#res_inscription.type andalso
              InscriptionPos =:= ReplaceInscriptionConfig#res_inscription.type -> ok;
            true ->
              ?FILE_LOG_ERROR("inscription type error, ~p, ~p, ~p", [InscriptionPos, ReplaceInscriptionConfig, InscriptionConfig]),
              throw({custom, "HintRequestDataError"})
          end
      end
  end.

check_backpack_inscription("", _) -> ok;
check_backpack_inscription(InscriptionID, InscriptionList) when is_list(InscriptionID) andalso is_list(InscriptionList) ->
  case cache_util:find_inscription_by_id(InscriptionList, InscriptionID) of
    {success, _} -> ok;
    fail ->
      ?FILE_LOG_ERROR("inscription not in backpack,~p, ~p", [InscriptionID, InscriptionList]),
      throw({custom, "HintRequestDataError"})
  end.

replace_inscription(Account, TowerID, InscriptionID, ReplaceID, InscriptionPos) ->
  {success, TowerItem} = cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, TowerID),
  {HeroInscriptionList, BackpackInscriptionList} =
    case ReplaceID of
      "" -> %%卸载
        {cache_util:delete_inscription_by_id(TowerItem#character.inscription_list, InscriptionID), [#inscription{id = InscriptionID, pos = InscriptionPos} | Account#account.backpack#backpack.inscription_list]};
      _ ->
        case InscriptionID of
          "" ->
            {[#inscription{id = ReplaceID, pos = InscriptionPos} | TowerItem#character.inscription_list], cache_util:delete_inscription_by_id(Account#account.backpack#backpack.inscription_list, ReplaceID)};
          _ ->
            BpInscriptionList = cache_util:delete_inscription_by_id(Account#account.backpack#backpack.inscription_list, ReplaceID),
            {cache_util:update_inscription_by_pos(TowerItem#character.inscription_list, #inscription{id = ReplaceID, pos = InscriptionPos}), [#inscription{id = InscriptionID, pos = InscriptionPos} | BpInscriptionList]}
        end
    end,

  NTower = TowerItem#character{inscription_list = HeroInscriptionList},
  Backpack = Account#account.backpack#backpack{inscription_list = BackpackInscriptionList},
  HeroList = cache_util:update_tower(Account#account.heros#heros.character_lists, NTower),
  Hero = Account#account.heros#heros{character_lists = HeroList},
  {Account#account{heros = Hero, backpack = Backpack}, ["hero", "backpack"], NTower}.

check_cdkey(Account, CDKey, CDKeyPackageConfig) when is_record(Account, account) andalso is_record(CDKey, cdkey_rd) andalso is_record(CDKeyPackageConfig, res_cdkey_package_config) ->
  %%校验验证码合理性包括：类型（全平台还是单平台），平台类型是否匹配，账户类型是否匹配
  {success, PName} = dd_ms:read_config(platform_name),
  PlatName = string:to_upper(dd_util:to_list(PName)),
  Plat =  string:to_upper(CDKey#cdkey_rd.plat_type),
  case CDKey#cdkey_rd.code_type of
    0 -> ok;
    1 ->
      if
        PlatName =:= Plat -> ok;
        true ->
          ?FILE_LOG_DEBUG("cdkey exchange => plat = ~p, cdkey plat = ~p", [PlatName, CDKey#cdkey_rd.plat_type]),
          throw({custom, "HintInvalidCDKEY"})
      end
  end,
  %%针对特殊的激活码做特殊处理
  CodeID = CDKey#cdkey_rd.code_id,
  case CDKeyPackageConfig#res_cdkey_package_config.spec_cdkey of
    "" -> ok;
    CodeID -> ok;
    _ -> throw({custom, "HintInvalidCDKEY"})
  end,
  %%针对单个账户的限制次数
  case CDKeyPackageConfig#res_cdkey_package_config.type of
    0 -> ok;
    Count ->
      case gb_trees:lookup(CDKeyPackageConfig#res_cdkey_package_config.id, Account#account.addition#addition.cdkey_pack_list) of
        {value, Num} ->
          if
            Num >= Count -> throw({custom, "HintGotTypeCDKEY"});
            true -> ok
          end;
        none -> ok
      end
  end.

inc_cdkey_package(Account, Package) when is_record(Account, account) andalso is_record(Package, res_cdkey_package_config) ->
  List =
    [
      {Package#res_cdkey_package_config.c1_type, Package#res_cdkey_package_config.c1_id, Package#res_cdkey_package_config.c1_count},
      {Package#res_cdkey_package_config.c2_type, Package#res_cdkey_package_config.c2_id, Package#res_cdkey_package_config.c2_count},
      {Package#res_cdkey_package_config.c3_type, Package#res_cdkey_package_config.c3_id, Package#res_cdkey_package_config.c3_count},
      {Package#res_cdkey_package_config.c4_type, Package#res_cdkey_package_config.c4_id, Package#res_cdkey_package_config.c4_count},
      {Package#res_cdkey_package_config.c5_type, Package#res_cdkey_package_config.c5_id, Package#res_cdkey_package_config.c5_count}
    ],
  %%礼包内容1类型（1=金币，2=钻石，3=体力，4=道具）
  {NAccount, PackageList} =
    lists:foldl(
      fun({Type, ID, Count}, {TmpAccount, TmpList}) ->
        case Type of
          1 ->
            {cache_api:account_inc_money(TmpAccount, 2, Count, "CDKEYPackageGift"), [{Type, ID, Count} | TmpList]};
          2 ->
            {cache_api:account_inc_money(TmpAccount, 1, Count, "CDKEYPackageGift"), [{Type, ID, Count} | TmpList]};
          3 ->
            {cache_api:inc_strength(TmpAccount, Count, "CDKEYPackageGift"), [{Type, ID, Count} | TmpList]};
          4 ->
            if
              Count > 0 ->
                cache_csv:get_property_config(ID),
                PropertyTree = cache_api:inc_tree_element([{ID, Count}], TmpAccount#account.backpack#backpack.prop_list),
                NBackpack = TmpAccount#account.backpack#backpack{prop_list = PropertyTree},
                {TmpAccount#account{backpack = NBackpack}, [{Type, ID, Count} | TmpList]};
              true ->
                {TmpAccount, TmpList}
            end;
          _ ->
            {TmpAccount, TmpList}
        end
      end, {Account, []}, List),

  NAddition =
    case Package#res_cdkey_package_config.type of
      0 -> Account#account.addition;
      Count ->
        PackTree =
          case gb_trees:lookup(Package#res_cdkey_package_config.id, Account#account.addition#addition.cdkey_pack_list) of
            {value, Num} ->
              if
                Num >= Count -> throw({custom, "HintGotTypeCDKEY"});
                true ->
                  gb_trees:update(Package#res_cdkey_package_config.id, Num + 1, Account#account.addition#addition.cdkey_pack_list)
              end;
            none ->
              gb_trees:insert(Package#res_cdkey_package_config.id, 1, Account#account.addition#addition.cdkey_pack_list)
          end,
        Account#account.addition#addition{cdkey_pack_list = PackTree}
    end,
  {NAccount#account{addition = NAddition}, PackageList}.


get_friend_uin_list(PlatformInfo) when is_record(PlatformInfo, platform_info) ->
  lists:map(
    fun(FriendItem) ->
      FriendItem#friend_item.uin
    end, PlatformInfo#platform_info.player_friends),
  [-1].

get_energy_gift_uin_list(Strength) when is_record(Strength, strength) ->
  lists:map(
    fun(StrengthItem) ->
      StrengthItem#strength_item.give_strength_dest
    end, Strength#strength.give_friend_strength).


get_user_basic_info(UinList) when is_list(UinList) ->
    lists:map(
      fun(ID) ->
        Uid = dd_util:to_integer(ID),
        {success, CacheNode} = get_cache_node(Uid),
        {success, RankNode} = dd_config:get_cfg(ranking_node),
        CurNode = node(),
        QueryResult =
          case CacheNode of
            CurNode -> cache:query_account(Uid, []);
            _ -> rpc:call(CacheNode, cache, query_account, [Uid, []])
          end,
        case QueryResult of
          {success, UserAccount} ->
            case rpc:call(RankNode, ranking_endless, get_endless_rank, [Uid]) of
              {success, {_, {Rank, _, _}}} ->
                {
                  Uid,
                  UserAccount#account.platform_info#platform_info.player_dis_name,
                  cache_api:get_user_atk(UserAccount#account.heros#heros.character_lists, cache_api:get_equipment_max_level(UserAccount)), %%战斗力
                  get_max_tollgate(UserAccount#account.stage#stage.base_tollgate_list),  %%闯关记录
                  UserAccount#account.stage#stage.endless_tollgate#endless_tollgate.max_score,  %%无尽最高分
                  Rank, %%无尽全服排名
                  UserAccount#account.heros                                              %%塔数据
                };
              OtherReason ->
                ?FILE_LOG_ERROR("query ranking [~p] server error, reason = ~p", [Uid, OtherReason]),
                throw({custom, "HintSystemDataError"})
            end;
          Other ->
            ?FILE_LOG_ERROR("query account[~p] error, reason = ~p", [Uid, Other]),
            throw({custom, "HintSystemDataError"})
        end
      end, UinList).

check_game_end_data_valid(Account, GameEndData) when is_record(Account, account) andalso is_record(GameEndData, game_end) ->
  TollgateID = GameEndData#game_end.tollgate_id,
  Stage = Account#account.stage,
  CurTs = dd_util:timestamp(),
  if
    TollgateID < 10000 ->
      case cache_util:find_finished_tollgate_by_id(Stage, TollgateID) of
        {success, Item} ->
          LastPassTs = Item#tollgate.last_pass_ts,
          Dif = CurTs - LastPassTs,
          if
            Dif < 10 -> throw({custom, "HintRepeatRequest"});
            true -> ok
          end;
        fail -> ok
      end;
    TollgateID < 20000 ->
      LastEndlessUpdateTs = Stage#stage.endless_tollgate#endless_tollgate.last_update_ts,
      EndLessDif = CurTs - LastEndlessUpdateTs,
      if
        EndLessDif < 10 -> throw({custom, "HintRepeatRequest"});
        true -> ok
      end;
    true ->
      case cache_util:find_activity_tollgate_by_id(Stage#stage.ac_tollgate, TollgateID) of
        {success, Item} ->
          LastAcPassTs = Item#activity_tollgate_item.last_finish_ts,
          AcDif = CurTs - LastAcPassTs,
          if
            AcDif < 10 -> throw({custom, "HintRepeatRequest"});
            true -> ok
          end;
        fail -> ok
      end
  end.


fast_purchase(Account, PurchaseType, Value) when is_record(Account, account) andalso is_integer(PurchaseType) ->
  CurCapacity = Account#account.backpack#backpack.capacity,
  case PurchaseType of
    1 -> %%体力
      CurStrengthValue = Account#account.strength#strength.strength,
      CurBuyTimes = Account#account.strength#strength.today_buy_times,
      CostList = [100,100,200,200,400,400,800,800],
      CostCount = lists:nth(CurBuyTimes + 1, CostList),
      NAccount = cache_api:account_dec_money(Account, 1, CostCount, "BuyStrengthCost"),
      NStrength = NAccount#account.strength#strength{today_buy_times = CurBuyTimes + 1, strength = CurStrengthValue + 100},

      %%写日志
      cache_log_util:write_consume_log(Account#account.uin, 5, "FastPurchaseStrength", [], 100, 1, CostCount, []),

      {NAccount#account{strength = NStrength}, ["gem", "strength"]};
    2 -> %%金币
      CostGem = 100,
      NAccount = cache_api:account_dec_money(Account, 1, CostGem, "FastBuyGoldCost"),
      NNAccount = cache_api:account_inc_money(NAccount, 2, 2500, "FastBuyGoldGet"),

      %%写日志
      cache_log_util:write_consume_log(Account#account.uin, 6, "FastPurchaseGold", [], 2500, 1, CostGem, []),

      {NNAccount, ["gold_coin", "gem"]};
    3 -> %%背包容量
      IntegerVal = dd_util:to_integer(Value),
      {CostGem, BuyCapacity} =
        if
          IntegerVal =< 0 -> {50, 10};
          true -> {IntegerVal*5, IntegerVal}
        end,
      NAccount = cache_api:account_dec_money(Account, 1, CostGem, "BuyBagCapacityCost"),
      NBackpack = NAccount#account.backpack#backpack{capacity = CurCapacity + BuyCapacity},
      NNAccount = NAccount#account{backpack = NBackpack},

      %%写日志
      cache_log_util:write_consume_log(Account#account.uin, 7, "FastPurchaseBackpackCapacity", [], 10, 1, CostGem, []),

      {NNAccount, ["backpack", "gem"]};
    Other ->
      ?FILE_LOG_ERROR("ERROR Buy Type", [Other]),
      throw({custom, "HintPurchaseError"})
  end.

%%type: 1体力，2金币，3背包容量
check_fast_purchase(Account, PurchaseType, _Value) when is_record(Account, account) andalso is_integer(PurchaseType) ->
  CurGem = Account#account.gem,
  case PurchaseType of
    1 -> %%体力
      CurBuyTimes = Account#account.strength#strength.today_buy_times,
      CostList = [100,100,200,200,400,400,800,800],
      CurStrengthVal = Account#account.strength#strength.strength,
      if
        CurStrengthVal >= 100 -> throw({custom, "HintSufficientEnergy"});
        CurBuyTimes >= 8 -> throw({custom, "HintNoPuchaseChance"});
        true ->
          %%检查钻石
          CostGem = lists:nth(CurBuyTimes + 1, CostList),
          if
            CostGem > CurGem -> throw({custom, "HintInsufficientDiamond"});
            true -> ok
          end
      end;
    2 -> %%金币
      CostGem = 100,
      if
        CostGem > CurGem -> throw({custom, "HintInsufficientDiamond"});
        true -> ok
      end;
    Other ->
      ?FILE_LOG_ERROR("ERROR purchase type", [Other]),
      throw({custom, "HintPurchaseError"})
  end.


settlement_friend_rank_reward(Uin, RankItem) when is_record(RankItem, rank_info) ->
  {success, MailNode} = dd_ms:read_config(mail_node),
  Rank = RankItem#rank_info.rank,
  Ret =
    if
      Rank < 0 -> success;
      Rank =:= 1 -> %%第一名
        rpc:call(MailNode, mail, friend_rank_reward, [Uin, 1]);
      Rank =< 2 -> %%第二名
        rpc:call(MailNode, mail, friend_rank_reward, [Uin, 2]);
      Rank =< 3 -> %%第三名
        rpc:call(MailNode, mail, friend_rank_reward, [Uin, 3]);
      true ->
        success
    end,
  case Ret of
    {success, _} -> success;
    success -> success;
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("friend rank mail reward error, reason = ~p, uin = ~p, rank = ~p", [Reason, Uin, RankItem]),
      fail;
    Other ->
      ?FILE_LOG_ERROR("friend rank mail error, reason = ~p", [Other]),
      fail
  end.


settlement_server_rank_reward(Account, Percent, Rank, Point) when is_record(Account, account) ->
  {success, MailNode} = dd_ms:read_config(mail_node),
  %%更新成绩
  EndlessTollgate = Account#account.stage#stage.endless_tollgate,
  NEndlessTollgate = Account#account.stage#stage.endless_tollgate#endless_tollgate{last_week_max_score = EndlessTollgate#endless_tollgate.this_week_max_score, last_week_max_score_gain_ts = EndlessTollgate#endless_tollgate.last_week_max_score_gain_ts, this_week_max_score = 0, this_week_max_score_gain_ts = 0, week_score_record = []},
  NStage = Account#account.stage#stage{endless_tollgate = NEndlessTollgate},
  NAddition = Account#account.addition#addition{rank_reward_got = 0},
  NAccount = Account#account{stage = NStage, addition = NAddition},
  Ret =
    if
      Percent =< 10 -> %%前百分之十
        rpc:call(MailNode, mail, server_rank_reward, [Account#account.uin, 10, Rank]);
      Percent =< 20 -> %%前百分之二十
        rpc:call(MailNode, mail, server_rank_reward, [Account#account.uin, 20, Rank]);
      Percent =< 30 -> %%前百分之三十
        rpc:call(MailNode, mail, server_rank_reward, [Account#account.uin, 30, Rank]);
      true ->
        success
    end,
  RetV =
    case Ret of
      {success, _} -> success;
      success -> success;
      {fail, Reason} ->
        ?FILE_LOG_DEBUG("server rank mail reward error, reason = ~p, uin = ~p, percent = ~p, rank = ~p, point = ~p", [Reason, Account#account.uin, Percent, Rank, Point]),
        fail;
      Other ->
        ?FILE_LOG_ERROR("server rank mail error, reason = ~p", [Other]),
        fail
    end,
  {NAccount, ["stage", "addition"], {RetV}}.

%%全部邮件： 1。好友邮件 2. 系统邮件
get_mass_attach_mail(Account, MailType) when is_record(Account, account) andalso is_integer(MailType) ->
  {success, MailNode} = dd_ms:read_config(mail_node),
  AttachMailList =
    case rpc:call(MailNode, mail, get_attach_mail_by_uin, [Account#account.uin]) of
      {success, MailList} -> MailList;
      {fail, Reason} ->
        throw({custom, Reason})
    end,
  case AttachMailList of
    [] -> {Account, [], {[], []}};
    _ ->
      %%领取奖励
      {MailIDList, NAccount, FieldList} =
        lists:foldr(
          fun(AttachMail, {TmpMailIDList, TmpAccount, TmpFieldList}) ->
            case AttachMail#attach_mail.mail_type of
              MailType ->
                case AttachMail#attach_mail.mail_attachment#mail_attachment.type of
                  1 -> %%金币
                    Account1 = cache_api:account_inc_money(TmpAccount, 2, AttachMail#attach_mail.mail_attachment#mail_attachment.count, "AttachMailGet"),
                    {[AttachMail#attach_mail.mail_id | TmpMailIDList], Account1, ["gold_coin" | TmpFieldList]};
                  2 -> %%钻石
                    Account1 = cache_api:account_inc_money(TmpAccount, 1, AttachMail#attach_mail.mail_attachment#mail_attachment.count, "AttachMailGet"),
                    {[AttachMail#attach_mail.mail_id | TmpMailIDList], Account1, ["gem" | TmpFieldList]};
                  3 -> %%体力
                    Account1 = cache_api:inc_strength(TmpAccount, AttachMail#attach_mail.mail_attachment#mail_attachment.count, "AttachMailGet"),
                    {[AttachMail#attach_mail.mail_id | TmpMailIDList], Account1, ["strength" | TmpFieldList]};
                  4 -> %%道具
                    ToolConfig = cache_csv:get_property_config(AttachMail#attach_mail.mail_attachment#mail_attachment.property_id),
                    NPropertyTree = cache_api:inc_tree_element([{ToolConfig#res_property.id, AttachMail#attach_mail.mail_attachment#mail_attachment.count}], TmpAccount#account.backpack#backpack.prop_list),
                    NBackpack = TmpAccount#account.backpack#backpack{prop_list = NPropertyTree},
                    {[AttachMail#attach_mail.mail_id | TmpMailIDList], TmpAccount#account{backpack = NBackpack}, ["backpack" | TmpFieldList]};
                  Other ->
                    ?FILE_LOG_ERROR("error attach type, mail = ~p", [Other, AttachMail]),
                    {TmpMailIDList, TmpAccount, TmpFieldList}
                end;
              _ -> {TmpMailIDList, TmpAccount, TmpFieldList}
            end
          end, {[], Account, []}, AttachMailList),

      %%删除邮件
      case MailIDList of
        [] -> ok;
        _ ->
          success = rpc:call(MailNode, mail, delete_mass_attach_mail, [MailIDList])
      end,

      {UpdateMissionList, UpdateAchievementList, NewAccount, TFieldList} =
        case MailType of
          1 ->
            Count = lists:foldr(fun(AM, TmpCnt) -> TmpCnt + AM#attach_mail.mail_attachment#mail_attachment.count end, 0, AttachMailList),
            {UMissonList, NMission} = cache_mission:update_mission(strength_gift, Account#account.uin, NAccount#account.mission, {2, Count}),
            {UMAchievementList, NAchievement} = cache_mission:update_achievement(strength_gift, Account#account.uin, NAccount#account.achievement, {2, Count}),
            {UMissonList, UMAchievementList, NAccount#account{mission = NMission, achievement = NAchievement}, ["mission", "achievement"]};
          _ -> {[], [], NAccount, []}
        end,
      NFieldList = dd_util:filter_string(lists:merge(FieldList, TFieldList)),
      {NewAccount, NFieldList, {UpdateMissionList, UpdateAchievementList}}
  end.

get_mail_attach(Account, MailID) when is_record(Account, account) andalso is_integer(MailID) ->
  {success, MailNode} = dd_ms:read_config(mail_node),
  AttachMail =
    case rpc:call(MailNode, mail, get_attach_mail, [MailID]) of
      not_exist -> throw({custom, "HintMailNotExist"});
      {success, Mail} -> Mail;
      {fail, Reason} ->
        throw({custom, Reason})
    end,
  %%领取奖励
  {NAccount, FieldList} =
    case AttachMail#attach_mail.mail_attachment#mail_attachment.type of
      1 -> %%金币
        Account1 = cache_api:account_inc_money(Account, 2, AttachMail#attach_mail.mail_attachment#mail_attachment.count, "AttachMailGet"),
        {Account1, ["gold_coin"]};
      2 -> %%钻石
        Account1 = cache_api:account_inc_money(Account, 1, AttachMail#attach_mail.mail_attachment#mail_attachment.count, "AttachMailGet"),
        {Account1, ["gem"]};
      3 -> %%体力
        Account1 = cache_api:inc_strength(Account, AttachMail#attach_mail.mail_attachment#mail_attachment.count, "AttachMailGet"),
        {Account1, ["strength"]};
      4 -> %%道具
        ToolConfig = cache_csv:get_property_config(AttachMail#attach_mail.mail_attachment#mail_attachment.property_id),
        NPropertyTree = cache_api:inc_tree_element([{ToolConfig#res_property.id, AttachMail#attach_mail.mail_attachment#mail_attachment.count}], Account#account.backpack#backpack.prop_list),
        NBackpack = Account#account.backpack#backpack{prop_list = NPropertyTree},
        {Account#account{backpack = NBackpack}, ["backpack"]};
      Other ->
        ?FILE_LOG_ERROR("error attach type", [Other]),
        throw({custom, "HintSystemDataError"})
    end,

  %%删除邮件
  success = rpc:call(MailNode, mail, del_attach_mail, [MailID]),

  %%更新成就或者任务
  MailType = AttachMail#attach_mail.mail_attachment#mail_attachment.type,
  Count = AttachMail#attach_mail.mail_attachment#mail_attachment.count,
  MailSource = AttachMail#attach_mail.mail_source,
  {UpdateMissionList, UpdateAchievementList, NewAccount, TFieldList} =
      if
        MailSource =/= -1 andalso MailType =:= 3 -> %%接收体力
          {UMissonList, NMission} = cache_mission:update_mission(strength_gift, Account#account.uin, NAccount#account.mission, {2, Count}),
          {UMAchievementList, NAchievement} = cache_mission:update_achievement(strength_gift, Account#account.uin, NAccount#account.achievement, {2, Count}),
          {UMissonList, UMAchievementList, NAccount#account{mission = NMission, achievement = NAchievement}, ["mission", "achievement"]};
        true ->
          {[], [], NAccount, []}
      end,
  NFieldList = lists:umerge3(FieldList, TFieldList, []),
  {NewAccount, NFieldList, {UpdateMissionList, UpdateAchievementList}}.


%%购买钻石
iap_buy(Account, Order, ProductID, ProductInfo) when is_record(Account, account) andalso is_list(Order) andalso is_list(ProductID) andalso is_record(ProductInfo, res_goods)->
  %%更新售卖次数
  RestrictCount = ProductInfo#res_goods.restrict_count,
  GoodsList =
    if
      RestrictCount > 0 ->
        case cache_util:find_commodity_by_id(Account#account.shop#shop.goods_list, ProductInfo#res_goods.id) of
          fail -> [#goods{id = ProductInfo#res_goods.id, remain_count = ProductInfo#res_goods.restrict_count - 1, latest_buy_ts = dd_util:timestamp()} | Account#account.shop#shop.goods_list];
          {success, Item} -> cache_util:update_commodity(Account#account.shop#shop.goods_list, Item#goods{remain_count = Item#goods.remain_count - 1, latest_buy_ts = dd_util:timestamp()})
        end;
      true -> Account#account.shop#shop.goods_list
    end,

  %%更新金币钻石或者道具
  Account1 =
    case ProductInfo#res_goods.goods_type of
      1 -> %%钻石
        BuyCount = ProductInfo#res_goods.goods_count,
        cache_api:account_inc_money(Account, 1, BuyCount, "Charge");
      Other -> %%其他
        ?FILE_LOG_INFO("buy other goods [~p]", [Other]),
        throw({custom, "ProductInfoError"})
    end,
  %%增加赠送
  Account3 =
    case ProductInfo#res_goods.gift_type of
      1 -> %%赠送钻石
        cache_api:account_inc_money(Account1, 1, ProductInfo#res_goods.gift_count, "BuyGoodsGift");
      2 -> %%赠送金币
        cache_api:account_inc_money(Account1, 2, ProductInfo#res_goods.gift_count, "BuyGoodsGift");
      OtherType ->
        ?FILE_LOG_INFO("buy goods gift other type [~p]", [OtherType]),
        Account1
    end,

  PayPrice = dd_util:list_to_float(ProductInfo#res_goods.money_count, 0.0),
  PayItem = #pay_item{pay_ts = dd_util:timestamp(), pay_item_id = ProductID, pay_gem_count = ProductInfo#res_goods.goods_count, pay_price = PayPrice},
  TotalPayVal = Account3#account.shop#shop.pay_info#pay_info.total_pay_val + PayPrice,
  TotalGemCount = Account3#account.shop#shop.pay_info#pay_info.total_gem_count + ProductInfo#res_goods.goods_count,
  NRecord = [PayItem | Account3#account.shop#shop.pay_info#pay_info.pay_record],
  NPayInfo =
    case Account3#account.shop#shop.pay_info#pay_info.first_pay_ts of
      0 ->
        Account3#account.shop#shop.pay_info#pay_info{first_pay_ts = dd_util:timestamp(), pay_record = NRecord, total_pay_val = TotalPayVal, total_gem_count = TotalGemCount};
      _ ->
        Account3#account.shop#shop.pay_info#pay_info{pay_record = NRecord, total_pay_val = TotalPayVal, total_gem_count = TotalGemCount}
    end,

  %%添加charge
  success = add_charge_order(Account#account.uin, Order),
  Shop = Account3#account.shop#shop{goods_list = GoodsList, pay_info = NPayInfo},
  {Account3#account{shop = Shop}, ["shop", "gold_coin", "gem"], {ProductInfo#res_goods.goods_count}}.

%%购买钻石检查
check_iap_valid(Account, Orders, ProductID, CommodityConfig) when is_record(Account, account) andalso is_list(Orders) andalso is_list(ProductID) andalso is_record(CommodityConfig, res_goods)->
  not_exist = get_charge_order(Account#account.uin, Orders),
  case CommodityConfig#res_goods.goods_type of
    1 -> %%钻石
      RestrictCount = CommodityConfig#res_goods.restrict_count,
      if
        RestrictCount > 0 -> %%检查 限购次数
          case cache_util:find_commodity_by_id(Account#account.shop#shop.goods_list, CommodityConfig#res_goods.id) of
            {success, GoodsItem} ->
              TRemainCount = GoodsItem#goods.remain_count,
              if
                TRemainCount > 0 -> ok;
                true ->
                  ?FILE_LOG_WARNING("buy goods: goods [~p] has no chance!", [CommodityConfig#res_goods.id]),
                  throw({custom, "HintNoPuchaseChance"})
              end;
            fail ->
              ?FILE_LOG_WARNING("commodity data error, [~p]", [CommodityConfig#res_goods.id]),
              ok
          end;
        true -> ok
      end,
      %%检查是否在销售期内
      CurTime = dd_util:timestamp(),
      StartTs =  CommodityConfig#res_goods.start_ts,
      OverTs =  CommodityConfig#res_goods.over_ts,
      if
        StartTs =< 0 -> ok; %%没有限购期
        StartTs > 0 andalso CurTime >= StartTs andalso CurTime =< OverTs -> ok;
        true ->
          ?FILE_LOG_WARNING("commodity not in the sales period.", []),
          throw({custom, "HintNotAllowPurchase"})
      end;
    Other ->
      ?FILE_LOG_DEBUG("purchase type error", [Other]),
      throw({custom, "HintPurchaseError"})
  end.


get_commodity_from_config_by_product_id(ProductID) ->
  ShopConfigList = cache_csv:get_all_shop_config(),
  lists:foldr(
    fun(GoodItem, TmpList) ->
      case GoodItem#res_goods.goods_type of
        1 ->
          TGoodsNo = GoodItem#res_goods.goods_no,
          if
            TGoodsNo =:= ProductID -> [GoodItem | TmpList];
            true -> TmpList
          end;
        _ -> TmpList
      end
    end, [], ShopConfigList).

lottery(Account, LotteryType) when is_record(Account, account) andalso is_integer(LotteryType) ->
  LotteryConfig = get_lottery_config(LotteryType, Account#account.lottery#lottery.single_lottery_times),
  EquipPieceConfig = cache_csv:get_equip_piece_config_by_id(LotteryConfig#lottery_conf.tool_id),
  {DropList, CostGem} =
    case LotteryType of
      1 -> %%单抽
        %%对单抽做特殊处理，新玩家首次抽奖的
        [LotteryTreasureItem] = gen_lottery_treasure(1, LotteryConfig#lottery_conf.id, dd_util:random_in_range(2, 10), dd_util:random_in_range(?MAX_SEED), []),
        [TreasureItem] = gen_treasure([LotteryTreasureItem], {Account, dd_util:random_in_range(?MAX_SEED)}, []),
        {[TreasureItem],LotteryConfig#lottery_conf.price};
      2 -> %%十连抽
        %%列表最后一位是十连抽必中元素
        LotteryTreasureList = gen_lottery_treasure(10, LotteryType, 1, dd_util:random_in_range(?MAX_SEED), []),
        TreasureList = gen_treasure(LotteryTreasureList, {Account, dd_util:random_in_range(?MAX_SEED)}, []),
        {TreasureList, LotteryConfig#lottery_conf.price};
      Other ->
        ?FILE_LOG_ERROR("player[~p] lottery type error, type = ~p", [Account#account.uin, Other]),
        throw({custom, "HintLotteryDataError"})
    end,
  ?FILE_LOG_DEBUG("player[~p], single lottery LotteryPrice is ~p",[Account#account.uin, CostGem]),
  %%扣除钻石
  NAccount = update_account_with_treasure_list(DropList, <<"Lottery">>, Account),
  NNAccount = cache_api:account_dec_money(NAccount, 1, CostGem, "LotteryCost"),
  %%添加材料
  EquipPieceList = cache_api:inc_tree_element([{EquipPieceConfig#res_equip_piece.id, 1}], NNAccount#account.backpack#backpack.equip_piece),
  NBackpack = NNAccount#account.backpack#backpack{equip_piece = EquipPieceList},
  NewAccount = NNAccount#account{backpack = NBackpack, lottery = increase_lottery_time(LotteryType, NNAccount#account.lottery)},
  %%写日志
  cache_log_util:write_lottery_log(Account#account.uin, LotteryType, 1, CostGem, DropList),
  ?FILE_LOG_INFO("[~p][~p][~p][~p]", [Account#account.uin, "Lottery", LotteryType, DropList]),
  {NewAccount, ["gem", "gold_coin", "backpack", "lottery", "hero"], {DropList}}.

%%单次抽奖次数增加
increase_lottery_time(LotteryType, Lottery)->
  case LotteryType of
    1 ->
      Lottery#lottery{single_lottery_times =  Lottery#lottery.single_lottery_times + 1};
    2 ->
      Lottery#lottery{ten_lottery_times =  Lottery#lottery.ten_lottery_times + 1};
    _ -> Lottery
  end.

%%必定掉落(掉落塔),在所有的塔都已经掉落了之后将掉落铭文，如果铭文也全部掉落了，则掉碎片
get_lottery_const_drop(Account, TreasureItem, HaveGotTreasureList) ->
  InscriptionClassTree =
    lists:foldl(
      fun(Treasure, TmpTree) ->
        case Treasure#treasure_item.drop_type of
          4 ->
            Config = cache_csv:get_inscription_by_id(Treasure#treasure_item.drop_id),
            case gb_trees:lookup(Config#res_inscription.inscription_class, TmpTree) of
              none -> gb_trees:insert(Config#res_inscription.inscription_class, 1, TmpTree);
              {value, _} -> TmpTree
            end;
          _ -> TmpTree
        end
      end, get_user_inscription_class(Account), HaveGotTreasureList),
  PrimaryInscriptionList =
    lists:foldl(
      fun(Config, TmpList) ->
        if
          Config#res_inscription.star =:= 1 ->
            case gb_trees:lookup(Config#res_inscription.inscription_class, InscriptionClassTree) of
              none ->
                case Config#res_inscription.belong_tower of
                  "TY" -> [Config | TmpList];
                  _ ->
                    case cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, Config#res_inscription.belong_tower) of
                      {success, _} -> [Config | TmpList];
                      fail -> TmpList
                    end
                end;
              {value, _} -> TmpList
            end;
          true -> TmpList
        end
      end,[], cache_csv:get_all_inscription_config()),
  get_lottery_const_drop_1(PrimaryInscriptionList, <<"inscription">>, TreasureItem, Account#account.heros#heros.character_lists).

get_lottery_const_drop_1([], <<"null">>, TreasureItem, _) ->
  TreasureItem#treasure_item{sub_id = 0, drop_id = "shard_01_01", drop_level = 0, drop_type = 2, count = 1};

get_lottery_const_drop_1([], <<"inscription">>, TreasureItem, HeroList) ->
  InscriptionPieceList = cache_csv:get_all_inscription_piece_config(),
  Len = length(InscriptionPieceList),
  PieceConfig = lists:nth(dd_util:random_in_range(1, Len), InscriptionPieceList),
  get_lottery_const_drop_1([PieceConfig], <<"piece">>, TreasureItem, HeroList);

get_lottery_const_drop_1([InscriptionC | _], <<"inscription">>, TreasureItem, _HeroList) ->
  TreasureItem#treasure_item{sub_id = 0, drop_id = InscriptionC#res_inscription.id, drop_level = 0, drop_type = 4, count = 1};

get_lottery_const_drop_1([], <<"piece">>, TreasureItem, HeroList) ->
  get_lottery_const_drop_1([], <<"null">>, TreasureItem, HeroList);

get_lottery_const_drop_1([Piece | T], <<"piece">>, TreasureItem, HeroList) ->
  case Piece#res_inscription_piece.belong_tower of
    "" ->
      TreasureItem#treasure_item{sub_id = 0, drop_id = Piece#res_inscription_piece.id, drop_level = 0, drop_type = 5, count = 1};
    TowerID ->
      case cache_util:find_tower_by_id(HeroList, TowerID) of
        {success, _} ->
          TreasureItem#treasure_item{sub_id = 0, drop_id = Piece#res_inscription_piece.id, drop_level = 0, drop_type = 5, count = 1};
        fail ->
          get_lottery_const_drop_1(T,  <<"piece">>, TreasureItem, HeroList)
      end
  end.

%%根据宝箱，生成宝箱内容
%%对特殊宝箱需要进行特殊处理，角色全满的情况下给铭文，铭文全满的情况下需要给铭文碎片
gen_treasure([], _, OutList) -> OutList;
gen_treasure([Item | T], {Account, Seed}, OutList) ->
  TreasureConfig = cache_csv:get_treasure_config_by_id(Item#treasure_item.treasure_id),
  TreasureID = Item#treasure_item.treasure_id,
  {TreasureItemList, TotalProb} = get_treasure_item_list(Account, OutList, TreasureConfig#res_treasure_config.item_list),
  ?FILE_LOG_DEBUG("gen treasure, total prob = ~p", [TotalProb]),
  if
    TotalProb =:= 0 andalso TreasureID =:= 5999 -> %%必掉宝箱
      gen_treasure(T, {Account, Seed}, [get_lottery_const_drop(Account, Item, OutList) | OutList]);
    TotalProb =< 0 -> throw({custom, "HintSystemDataError"});
    true ->
      RandV = dd_util:random_in_range(Seed) rem TotalProb,
      ?FILE_LOG_DEBUG("gen treasure, total prob = ~p, randv = ~p", [TotalProb, RandV]),
      {success, TreasureItem} = compare_treasure_prob(TreasureItemList, RandV),
      NItem = Item#treasure_item{drop_id = TreasureItem#res_treasure_item_config.item_id, sub_id = TreasureItem#res_treasure_item_config.sub_drop_id,
        drop_type = TreasureItem#res_treasure_item_config.item_type, count = TreasureItem#res_treasure_item_config.item_count, drop_level = TreasureItem#res_treasure_item_config.item_level},
      gen_treasure(T, {Account, Seed}, [NItem | OutList])
  end.

%%生成宝箱
gen_lottery_treasure(0, _LotteryType, _RandV, _Seed, OutList) -> lists:reverse(OutList);
gen_lottery_treasure(RandV, LotteryType, RandV, Seed, OutList) ->     %%掉落紫色装备
  LotteryConfig = cache_csv:get_lottery_config_by_id(LotteryType),
  F = fun(TItem, TID) -> TItem#res_lottery_item_config.lottery_id =:= TID end,
  {success, LotteryItem} = cache_util:find_item_by_id(LotteryConfig#res_lottery_config.lottery_item_lists, F, 5999),
  Count = counting_lottery_by_id(OutList, 5999, 0),
  Limit = LotteryItem#res_lottery_item_config.lottery_limit,
  if
    Limit > 0 andalso Count >= LotteryItem#res_lottery_item_config.lottery_limit ->
      gen_lottery_treasure(RandV, LotteryType, 0, Seed, OutList);  %%已经存在了必掉物品，随机生成
    true ->
      NItem = #treasure_item{treasure_id = 5999, drop_id = "", drop_type = 0, sub_id = 0, count = 0, drop_level = 0},
      gen_lottery_treasure(RandV - 1, LotteryType, RandV, Seed, [NItem | OutList])
  end;
gen_lottery_treasure(RemainN, LotteryType, RandV, Seed, OutList) ->
  LotteryConfig = cache_csv:get_lottery_config_by_id(LotteryType),
  RandValue = dd_util:random_in_range(Seed) rem 10000,
  {success, LotteryItem} = compare_lottery_prob(LotteryConfig#res_lottery_config.lottery_item_lists, RandValue),
  Count = counting_lottery_by_id(OutList, LotteryItem#res_lottery_item_config.lottery_id, 0),
  if
    LotteryItem#res_lottery_item_config.lottery_limit > 0 andalso Count >= LotteryItem#res_lottery_item_config.lottery_limit -> gen_lottery_treasure(RemainN, LotteryType, RandV, Seed, OutList);
    true ->
      NItem = #treasure_item{treasure_id = LotteryItem#res_lottery_item_config.lottery_id, drop_id = "", drop_type = 0, sub_id = 0, count = 0, drop_level = 0},
      gen_lottery_treasure(RemainN - 1, LotteryType, RandV, Seed, [NItem | OutList])
  end.

counting_lottery_by_id([], _ID, Count) -> Count;
counting_lottery_by_id([Item | T], ID, Count) ->
  if
    ID =:= Item#treasure_item.treasure_id -> counting_lottery_by_id(T, ID, Count + 1);
    true -> counting_lottery_by_id(T, ID, Count)
  end.

%%生成宝箱id
compare_lottery_prob([], _RandValue) -> fail;
compare_lottery_prob([Item | T], RandValue) ->
  if
    RandValue >= Item#res_lottery_item_config.prob_low_value andalso RandValue =< Item#res_lottery_item_config.prob_up_value -> {success, Item};
    true -> compare_lottery_prob(T, RandValue)
  end.

check_lottery_valid(Account, LotteryType) when is_record(Account, account) andalso is_integer(LotteryType) ->
  LotteryConfig = get_lottery_config(LotteryType, Account#account.lottery#lottery.single_lottery_times),   %格式为lottery_conf字段 列表
  CurrentGem = Account#account.gem,
  if
    CurrentGem < LotteryConfig#lottery_conf.price ->
      ?FILE_LOG_ERROR("player[~p] gem not enough, gem[~p], price[~p]",[Account#account.uin, CurrentGem, LotteryConfig#lottery_conf.price]),
      throw({custom, "HintInsufficientDiamond"});
    true -> ok
  end.


%%无尽模式 (0表示正常操作，-1 表示退出或者restart)
execute_game_end(2, Account, OGameEndData) when is_record(Account, account) andalso is_record(OGameEndData, game_end) ->
  case OGameEndData#game_end.success of
    0 ->         %%normal
      %%检查掉落(2015-1-8 改为：不校验客户端掉落，全部由服务端控制)
      %%check_drops(Account, GameEndData#game_end.gain_drops, GameEndData#game_end.tollgate_id, GameEndData#game_end.endless_tollgate_num),
      {success, Drops} = calculate_drops(Account#account.stage, OGameEndData#game_end.tollgate_id, OGameEndData#game_end.endless_tollgate_num),
      GameEndData = OGameEndData#game_end{gain_drops = Drops},
      %%检查掉落 (无尽是按照boss个数  2014-12-19)
      %%check_drops(Account, GameEndData#game_end.gain_drops, GameEndData#game_end.tollgate_id, GameEndData#game_end.endless_tollgate_num - 1),
      %%更新关卡信息(返回stage和更新的关卡)
      {NStage, UpdateItem} = update_stage_with_game_end_data(0, Account#account.uin, Account#account.stage, GameEndData#game_end.tollgate_id, GameEndData#game_end.gain_star, GameEndData#game_end.gain_star_seq, GameEndData#game_end.gain_score, GameEndData#game_end.endless_tollgate_num),
      %%更新任务列表
      {UpdateMissionList, NMission} = cache_mission:update_mission(game_end, Account#account.uin, Account#account.mission, GameEndData),
      %%更新成就
      {UpdateAchievementGroupList, NAchievement} = cache_mission:update_achievement(game_end, Account#account.uin, Account#account.achievement, GameEndData),
      %%更新玩家基本数据包括体力，金币 （必须要使用未更新的account，因为要检查体力）
      NAccount = update_player_data_game_end(Account, GameEndData#game_end.gain_gold, GameEndData#game_end.tollgate_id),

      %%更新account
      NewAccount1 = NAccount#account{stage = NStage, mission = NMission, achievement = NAchievement},

      %%更新掉落和道具
      {Account1, DropItemList} = update_endless_drops_props(NewAccount1, GameEndData),

      FieldList = ["gold_coin", "backpack", "stage", "strength", "mission", "achievement"],
      {success, Account1, FieldList, {UpdateMissionList, UpdateAchievementGroupList, UpdateItem, DropItemList}};
    _ ->
      %%
      {NStage, UpdateItem} = update_stage_with_game_end_data(1, Account#account.uin, Account#account.stage, OGameEndData#game_end.tollgate_id, 0, [-1, -1, -1], OGameEndData#game_end.gain_score, 0),
      %%更新背包
      {NewAccount1, _} = update_backpack_with_props_drops(Account, OGameEndData#game_end.use_props, []),
      %%更新任务列表
      {UpdateMissionList, NMission} = cache_mission:update_mission(game_end, Account#account.uin, NewAccount1#account.mission, OGameEndData),
      %%更新成就
      {UpdateAchievementGroupList, NAchievement} =cache_mission:update_achievement(game_end, Account#account.uin, NewAccount1#account.achievement, OGameEndData),
      Account2 = NewAccount1#account{mission = NMission, achievement = NAchievement, stage = NStage},
      FieldList1 =  ["backpack", "mission", "achievement", "stage"],
      {success, Account2, FieldList1, {UpdateMissionList, UpdateAchievementGroupList, UpdateItem, []}}
  end;

execute_game_end(_, Account, OGameEndData) when is_record(Account, account) andalso is_record(OGameEndData, game_end) ->
  case OGameEndData#game_end.success of
    0 ->         %%成功
      %%检查掉落
      %%check_drops(Account, GameEndData#game_end.gain_drops, GameEndData#game_end.tollgate_id, GameEndData#game_end.endless_tollgate_num),
      {success, Drops} = calculate_drops(Account#account.stage, OGameEndData#game_end.tollgate_id, OGameEndData#game_end.endless_tollgate_num),
      GameEndData = OGameEndData#game_end{gain_drops = Drops},
      %%检查得分和星级
      check_star_score(GameEndData#game_end.data_statistics, GameEndData#game_end.gain_score, GameEndData#game_end.gain_star),
      %%更新关卡信息
      {NStage, UpdateItem} = update_stage_with_game_end_data(0, Account#account.uin, Account#account.stage, GameEndData#game_end.tollgate_id, GameEndData#game_end.gain_star, GameEndData#game_end.gain_star_seq, GameEndData#game_end.gain_score, 0),
      %%更新任务列表
      {UpdateMissionList, NMission} = cache_mission:update_mission(game_end, Account#account.uin, Account#account.mission, GameEndData),
      %%更新成就
      {UpdateAchievementGroupList, NAchievement} = cache_mission:update_achievement(game_end, Account#account.uin, Account#account.achievement, GameEndData),
      %%更新背包
      {NAccount, DropItem} = update_backpack_with_props_drops(Account, GameEndData#game_end.use_props, GameEndData#game_end.gain_drops),
      %%更新玩家基本数据包括体力，金币
      NNAccount = update_player_data_game_end(NAccount, GameEndData#game_end.gain_gold, GameEndData#game_end.tollgate_id),

      NewAccount1 = NNAccount#account{stage = NStage, mission = NMission, achievement = NAchievement},
      FieldList = ["gold_coin", "backpack","stage", "strength", "mission", "achievement"],

      %%获取排名
      {success, TollgateRank} = cache_ranking:get_friend_tollgate_rank(NAccount#account.uin, GameEndData#game_end.tollgate_id, get_friend_uin_list(NAccount#account.platform_info)),

      {success, NewAccount1, FieldList, {UpdateMissionList, UpdateAchievementGroupList, UpdateItem, TollgateRank, DropItem}};

    _ ->         %%失败：处理数据统计
      %%
      {NStage, UpdateItem} = update_stage_with_game_end_data(1, Account#account.uin, Account#account.stage, OGameEndData#game_end.tollgate_id, 0, [-1, -1, -1], OGameEndData#game_end.gain_score, 0),
      %%更新背包
      {NAccount, _} = update_backpack_with_props_drops(Account, OGameEndData#game_end.use_props, []),
      %%更新任务列表
      {UpdateMissionList, NMission} = cache_mission:update_mission(game_end, NAccount#account.uin, NAccount#account.mission, OGameEndData),
      %%更新成就
      {UpdateAchievementGroupList, NAchievement} =cache_mission:update_achievement(game_end, NAccount#account.uin, NAccount#account.achievement, OGameEndData),
      Account2 = NAccount#account{mission = NMission, achievement = NAchievement, stage = NStage},
      FieldList1 =  ["backpack", "mission", "achievement", "stage"],
      {success, TollgateRank} = cache_ranking:get_friend_tollgate_rank(Account2#account.uin, OGameEndData#game_end.tollgate_id, get_friend_uin_list(NAccount#account.platform_info)),
      {success, Account2, FieldList1, {UpdateMissionList, UpdateAchievementGroupList, UpdateItem, TollgateRank, []}}
  end.

%%获取关卡掉落的随机数
get_tollgate_drop_germ(Account, TollgateID) ->
  ?FILE_LOG_DEBUG("~p enter tollgate ~p", [Account#account.uin, TollgateID]),
  Seed = dd_util:random_in_range(?MAX_SEED) + ?MAX_SEED,
  RandV = dd_util:get_rand_val(Account#account.stage#stage.tollgate_drop#tollgate_drop.rand_val),
  CurTime = dd_util:timestamp(),
  TollgateDrop = #tollgate_drop{seed_val = Seed, rand_val = RandV, seed_ts = CurTime, end_ts = 0},
  Stage = Account#account.stage#stage{tollgate_drop = TollgateDrop},
  NAccount = Account#account{stage = Stage},
  {NAccount, ["stage"], {Seed, RandV}}.


generate_endless_drop_germ(Account) when is_record(Account, account) ->
  Seed = dd_util:random_in_range(?MAX_SEED) + ?MAX_SEED,
  RandV = dd_util:get_rand_val(Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.rand_val),
  CurTime = dd_util:timestamp(),
  EndlessDrop = Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop{seed_val = Seed, rand_val = RandV, seed_ts = CurTime, end_ts = 0},
  TodayRemainCount = 20 - length(EndlessDrop#endless_drop.endless_drop),
  Endless =  Account#account.stage#stage.endless_tollgate#endless_tollgate{endless_drop = EndlessDrop},
  Stage = Account#account.stage#stage{endless_tollgate = Endless},
  NAccount = Account#account{stage = Stage},
  {NAccount, ["stage"], {TodayRemainCount}}.

present_friend_strength(Account, FriendUin) when is_record(Account, account) andalso is_integer(FriendUin) ->
  {success, MailNode} = dd_ms:read_config(mail_node),
  case rpc:call(MailNode, mail, present_friend_strength, [Account#account.uin, FriendUin]) of
    {success, _} ->
      StrengthItem = #strength_item{give_strength_dest = FriendUin, give_strength_source = Account#account.uin, give_strength_value = 1, give_strength_ts = dd_util:timestamp()},
      StrengthItemList = [StrengthItem | Account#account.strength#strength.give_friend_strength],
      NStrength = Account#account.strength#strength{give_friend_strength = StrengthItemList},
      {Account#account{strength = NStrength}, ["strength"]};
    {fail, Reason} -> throw({custom, Reason})
  end.

check_friend_valid(Account, FriendUin) when is_integer(FriendUin) ->
%%  case cache_util:find_friend_by_uin(Account#account.platform_info#platform_info.player_friends, FriendUin) of
%%    {success, _FriendItem} -> ok;
%%    fail -> throw({custom, "HintNotFriends"})
%%  end,
  case cache_util:find_friend_strength_gift_by_id(Account#account.strength#strength.give_friend_strength, FriendUin) of
    {success, _Item} -> throw({custom, "HintHaveGiftEnergy"});
    fail -> ok
  end,
  {success, CacheNode} = get_cache_node(FriendUin),
  CurNode = node(),
  QueryResult =
    case CacheNode of
      CurNode -> cache:query_account(FriendUin, ["strength"]);
      _ -> rpc:call(CacheNode, cache, query_account, [FriendUin, ["strength"]])
    end,
  case QueryResult of
    {success, [{"strength", FriendStrength}]} ->
      if
        FriendStrength#strength.close_strength_gift =:= 0 ->
          ?FILE_LOG_ERROR("friend gift strength close, friend uin = ~p", [FriendUin]),
          throw({custom, "HintClosedEnergyGift"});
        true -> ok
      end;
    {fail, Reason} ->
      ?FILE_LOG_ERROR("query friend account error,reason = ~p", [Reason]),
      throw({custom, "HintSystemDataError"})
  end.



get_achievement_reward(Account, GroupID, AchievementID) when is_record(Account, account) andalso is_integer(GroupID) andalso is_list(AchievementID) ->
  case cache_util:find_achievement_group_by_id(Account#account.achievement#achievement.group_list, GroupID) of
    {success, AchievementGroup} ->
      LevelID1 =  AchievementGroup#achievement_group.item_level_1#achievement_item.id,
      LevelID2 = AchievementGroup#achievement_group.item_level_2#achievement_item.id,
      LevelID3 = AchievementGroup#achievement_group.item_level_3#achievement_item.id,
      {AchievementItem, Level} =
        case AchievementID of
          LevelID1 -> {AchievementGroup#achievement_group.item_level_1, 1};
          LevelID2 -> {AchievementGroup#achievement_group.item_level_2, 2};
          LevelID3 -> {AchievementGroup#achievement_group.item_level_3, 3};
          _ ->
            ?FILE_LOG_ERROR("achievement not exist: ~p", [AchievementID]),
            throw({custom, "HintSystemDataError"})
        end,
      RewardConfig = cache_csv:get_achievement_config(AchievementID),
      Account3 = obtain_reward("Achievement", Account, RewardConfig#res_achievement.reward_type, RewardConfig#res_achievement.reward_property_id, RewardConfig#res_achievement.reward_num),
      NAchievementItem = AchievementItem#achievement_item{get_reward_ts = dd_util:timestamp()},
      AG =
        case Level of
          1 ->  AchievementGroup#achievement_group{item_level_1 = NAchievementItem};
          2 -> AchievementGroup#achievement_group{item_level_2 = NAchievementItem};
          3 -> AchievementGroup#achievement_group{item_level_3 = NAchievementItem}
        end,

      List = cache_util:update_achievement_group(Account3#account.achievement#achievement.group_list, AG),
      NAchievement = Account3#account.achievement#achievement{group_list = List},

      %%写日志
      RewardDesc = dd_util:to_list(AchievementItem#achievement_item.id),
      cache_log_util:write_reward_log(Account#account.uin, "Achievement",  AchievementItem#achievement_item.finish_ts,
        RewardConfig#res_achievement.reward_type, RewardConfig#res_achievement.reward_property_id, RewardConfig#res_achievement.reward_num, RewardDesc),

      {Account3#account{achievement = NAchievement}, ["achievement"], AG};
    fail ->
      ?FILE_LOG_ERROR("achievement not exist: ~p", [AchievementID]),
      throw({custom, "HintSystemDataError"})
  end.

check_achievement_reward_valid(Account, GroupID, AchievementID) when is_record(Account, account) andalso is_integer(GroupID) andalso is_list(AchievementID) ->
  case cache_util:find_achievement_group_by_id(Account#account.achievement#achievement.group_list, GroupID) of
    {success, AchievementGroup} ->
      LevelID1 =  AchievementGroup#achievement_group.item_level_1#achievement_item.id,
      LevelID2 = AchievementGroup#achievement_group.item_level_2#achievement_item.id,
      LevelID3 = AchievementGroup#achievement_group.item_level_3#achievement_item.id,
       case AchievementID of
         LevelID1 ->
           FinishTs = AchievementGroup#achievement_group.item_level_1#achievement_item.finish_ts,
           GetRewardTs = AchievementGroup#achievement_group.item_level_1#achievement_item.get_reward_ts,
           if
             GetRewardTs > 0 ->
               ?FILE_LOG_ERROR("achievement has been draw => ~p", [AchievementID]),
               throw({custom, "HintAchievementDrawn"});
             FinishTs =< 0 ->
               ?FILE_LOG_ERROR("achievement has not been finish => ~p", [AchievementID]),
               throw({custom, "HintAchievementUnfinish"});
             true ->
               ok
           end;
         LevelID2 ->
           FinishTs = AchievementGroup#achievement_group.item_level_2#achievement_item.finish_ts,
           GetRewardTs = AchievementGroup#achievement_group.item_level_2#achievement_item.get_reward_ts,
           if
             GetRewardTs > 0 ->
               ?FILE_LOG_ERROR("achievement has been draw => ~p", [AchievementID]),
               throw({custom, "HintAchievementDrawn"});
             FinishTs =< 0 ->
               ?FILE_LOG_ERROR("achievement has not been finish => ~p", [AchievementID]),
               throw({custom, "HintAchievementUnfinish"});
             true ->
               ok
           end;
         LevelID3 ->
           FinishTs = AchievementGroup#achievement_group.item_level_3#achievement_item.finish_ts,
           GetRewardTs = AchievementGroup#achievement_group.item_level_3#achievement_item.get_reward_ts,
           if
             GetRewardTs > 0 ->
               ?FILE_LOG_ERROR("achievement has been draw => ~p", [AchievementID]),
               throw({custom, "HintAchievementDrawn"});
             FinishTs =< 0 ->
               ?FILE_LOG_ERROR("achievement has not been finish => ~p", [AchievementID]),
               throw({custom, "HintAchievementUnfinish"});
             true ->
               ok
           end;
         _ ->
           ?FILE_LOG_ERROR("achievement not exist: ~p", [AchievementID]),
           throw({custom, "HintSystemDataError"})
       end;
    fail ->
      ?FILE_LOG_ERROR("achievement not exist: ~p", [AchievementID]),
      throw({custom, "HintSystemDataError"})
  end.

obtain_reward(Type, Account, RewardType, RewardID, RewardNum) when is_integer(RewardType) and is_list(RewardID) andalso is_integer(RewardNum) andalso is_list(Type) ->
  case RewardType of
    1 -> %%金币
      cache_api:account_inc_money(Account, 2, RewardNum, Type ++ "RewardGold");
    2 -> %%钻石
      cache_api:account_inc_money(Account, 1, RewardNum, Type ++ "RewardGem");
    4 -> %%道具
      case RewardID of
        [] ->
          ?FILE_LOG_ERROR("~p Reward propertyID is null", [Type]),
          Account;
        _ ->
          PropList = [{RewardID, RewardNum}],
          NPropertyTree = cache_api:inc_tree_element(PropList, Account#account.backpack#backpack.prop_list),
          NBackpack = Account#account.backpack#backpack{prop_list = NPropertyTree},
          Account#account{backpack = NBackpack}
      end;
    5 -> %%体力
      cache_api:inc_strength(Account, RewardNum, Type ++ "RewardStrength");
    Other ->
      ?FILE_LOG_ERROR("obtain reward =>  error type = [~p] ", [Other]),
      Account
end.

get_mission_reward(Account, MissionID) when is_record(Account, account) andalso is_list(MissionID) ->
  case cache_util:find_mission_by_id(Account#account.mission#mission.mission_list, MissionID) of
    {success, MissionItem} ->
      RewardConfig = cache_csv:get_mission_config(MissionID),
      Account3 = obtain_reward("Mission", Account, RewardConfig#res_task.reward_type, RewardConfig#res_task.reward_property_id, RewardConfig#res_task.reward_num),
      List = cache_util:update_mission(Account3#account.mission#mission.mission_list, MissionItem#mission_item{mission_get_reward_ts = dd_util:timestamp()}),
      NMission = Account3#account.mission#mission{mission_list = List},

      %%写日志
      RewardDesc = MissionItem#mission_item.mission_id,
      cache_log_util:write_reward_log(Account#account.uin, "Mission", MissionItem#mission_item.mission_finish_ts, RewardConfig#res_task.reward_type,
        RewardConfig#res_task.reward_property_id, RewardConfig#res_task.reward_num, RewardDesc),

      {Account3#account{mission = NMission}, ["mission"], MissionItem#mission_item{mission_get_reward_ts = dd_util:timestamp()}};
    fail ->
      ?FILE_LOG_ERROR("mission not exist: ~p", [MissionID]),
      throw({custom, "HintSystemDataError"})
  end.

check_mission_reward_valid(Account, MissionID) when is_record(Account, account) andalso is_list(MissionID) ->
  case cache_util:find_mission_by_id(Account#account.mission#mission.mission_list, MissionID) of
    {success, MissionItem} ->
      case MissionItem#mission_item.mission_type of
        3 -> %%活跃度任务
          throw({custom, "HintRequestDataError"});
        _ ->
          TGetRewardTs = MissionItem#mission_item.mission_get_reward_ts,
          TFinishTs = MissionItem#mission_item.mission_finish_ts,
          if
            TGetRewardTs > 0 ->
              ?FILE_LOG_ERROR("mission has been draw => ~p", [MissionID]),
              throw({custom, "HintMissionDrawn"});
            TFinishTs =< 0 ->
              ?FILE_LOG_ERROR("mission has not been finish => ~p, MissionItem = ~p", [MissionID, MissionItem]),
              throw({custom, "HintMissionUnfinish"});
            true ->
              ok
          end
      end;
    fail ->
      ?FILE_LOG_ERROR("mission not exist: ~p", [MissionID]),
      throw({custom, "HintSystemDataError"})
  end.

get_activity_reward(Account, ActivityID) when is_record(Account, account) andalso is_integer(ActivityID) ->
  F = fun(Item, ID) -> Item#activity_reward_item.id =:= ID end,
  case cache_util:find_item_by_id(Account#account.mission#mission.player_activity#player_activity.activity_reward_list, F, ActivityID) of
    {success, ActivityRewardItem} ->
      RewardConfig = cache_csv:get_activity_reward_config(ActivityID),
      Account3 = obtain_reward("Activity", Account, RewardConfig#res_activity_reward.reward_type, RewardConfig#res_activity_reward.reward_property_id, RewardConfig#res_activity_reward.reward_num),
      F1 = fun(OItem, NItem) -> OItem#activity_reward_item.id =:= NItem#activity_reward_item.id end,
      List = cache_util:update_list_item(Account3#account.mission#mission.player_activity#player_activity.activity_reward_list, F1, ActivityRewardItem#activity_reward_item{get_reward_ts = dd_util:timestamp()}),
      NPlayer = Account3#account.mission#mission.player_activity#player_activity{activity_reward_list = List},
      NMission = Account3#account.mission#mission{player_activity = NPlayer},

      %%写日志
      RewardDesc = dd_util:to_list(ActivityRewardItem#activity_reward_item.id),
      cache_log_util:write_reward_log(Account#account.uin, "Activity", ActivityRewardItem#activity_reward_item.finish_ts, RewardConfig#res_activity_reward.reward_type,
        RewardConfig#res_activity_reward.reward_property_id, RewardConfig#res_activity_reward.reward_num, RewardDesc),

      {Account3#account{mission = NMission}, ["mission"]};
    fail ->
      ?FILE_LOG_ERROR("activity reward has not been finish: ~p", [ActivityID]),
      throw({custom, "HintActivityRewardUnfinish"})
  end.
check_activity_reward_valid(Account, ActivityID) when is_record(Account, account) andalso is_integer(ActivityID) ->
  F = fun(Item, ID) -> Item#activity_reward_item.id =:= ID end,
  case cache_util:find_item_by_id(Account#account.mission#mission.player_activity#player_activity.activity_reward_list, F, ActivityID) of
    {success, ActivityRewardItem} ->
      if
        ActivityRewardItem#activity_reward_item.get_reward_ts > 0 ->
          ?FILE_LOG_ERROR("activity reward has been draw => ~p", [ActivityID]),
          throw({custom, "HintActivityRewardDrawn"});
        ActivityRewardItem#activity_reward_item.finish_ts =< 0 ->
          ?FILE_LOG_ERROR("activity has not been finish => ~p", [ActivityID]),
          throw({custom, "HintActivityRewardUnfinish"});
        true ->
          ok
      end;
    fail ->
      ?FILE_LOG_ERROR("activity reward has not been finish: ~p", [ActivityID]),
      throw({custom, "HintActivityRewardUnfinish"})
  end.

buy_goods(Account, Uin, GoodsId, _Ip) when is_record(Account,account) andalso is_integer(Uin) andalso is_list(GoodsId)  ->
  CommodityConfig = cache_csv:get_commodity_config(GoodsId),
  %%更新售卖次数
  RestrictCount = CommodityConfig#res_goods.restrict_count,
  DailyGoodsList =
    if
      RestrictCount > 0 andalso CommodityConfig#res_goods.refresh_type =:= 1 ->
        case cache_util:find_commodity_by_id(Account#account.shop#shop.goods_list, GoodsId) of
          fail -> [#goods{id = GoodsId, remain_count = CommodityConfig#res_goods.restrict_count - 1, latest_buy_ts = dd_util:timestamp()} | Account#account.shop#shop.goods_list];
          {success, Item} -> cache_util:update_commodity(Account#account.shop#shop.goods_list, Item#goods{remain_count = Item#goods.remain_count - 1, latest_buy_ts = dd_util:timestamp()})
        end;
      true -> Account#account.shop#shop.goods_list
    end,
  PermanentGoodsList =
    if
      RestrictCount > 0 andalso CommodityConfig#res_goods.refresh_type =:= 2 ->
        case cache_util:find_commodity_by_id(Account#account.shop#shop.permanent_buy_list, GoodsId) of
          fail -> [#goods{id = GoodsId, remain_count = CommodityConfig#res_goods.restrict_count - 1, latest_buy_ts = dd_util:timestamp()} | Account#account.shop#shop.permanent_buy_list];
          {success, PermanentItem} -> cache_util:update_commodity(Account#account.shop#shop.permanent_buy_list, PermanentItem#goods{remain_count = PermanentItem#goods.remain_count - 1, latest_buy_ts = dd_util:timestamp()})
        end;
      true -> Account#account.shop#shop.permanent_buy_list
    end,

  %%更新金币钻石或者道具
  {Account1, BuyReason} =
    case CommodityConfig#res_goods.goods_type of
      1 -> %%钻石
        throw({custom, "HintInvalidRequest"});
      2 -> %%金币
        BuyCount = CommodityConfig#res_goods.goods_count,
        {cache_api:account_inc_money(Account, 2, BuyCount, "BuyGoldcoins"), "BuyGoldCoins"};
      3 -> %%道具礼包
        %%BuyCount = CommodityConfig#res_goods.goods_count,
        PackageNo = CommodityConfig#res_goods.goods_no,
        PackageConfig = cache_csv:get_shop_package_config_by_id(PackageNo),
        {inc_shop_package(Account, PackageConfig), "BuyShopPropertyPackage"};
      4 ->  %%道具
        BuyCount = CommodityConfig#res_goods.goods_count,
        PropertyNo = CommodityConfig#res_goods.goods_no,
        cache_csv:get_property_config(PropertyNo),
        PropertyTree = cache_api:inc_tree_element([{PropertyNo, BuyCount}], Account#account.backpack#backpack.prop_list),
        NBackpack = Account#account.backpack#backpack{prop_list = PropertyTree},
        {Account#account{backpack = NBackpack}, "BuyProperty"};
      5 -> %%体力
        BuyCount = CommodityConfig#res_goods.goods_count,
        {cache_api:inc_strength(Account, BuyCount, "BuyShopStrength"), "BuyShopStrength"};
      Other -> %%其他
        ?FILE_LOG_INFO("buy other goods [~p]", [Other]),
        {Account, "UnKnowBuyType"}
    end,
  CostCount = dd_util:to_integer(CommodityConfig#res_goods.money_count),
  Account2 =
    case CommodityConfig#res_goods.money_type of
      1 -> %%消耗钻石
        cache_api:account_dec_money(Account1, 1, CostCount, BuyReason);
      2 -> %%消耗金币
        cache_api:account_dec_money(Account1, 2, CostCount, BuyReason)
    end,
  %%增加赠送
  Account3 =
    case CommodityConfig#res_goods.gift_type of
      1 -> %%赠送钻石
        cache_api:account_inc_money(Account2, 1, CommodityConfig#res_goods.gift_count, "BuyGoodsGift");
      2 -> %%赠送金币
        cache_api:account_inc_money(Account2, 2, CommodityConfig#res_goods.gift_count, "BuyGoodsGift");
      3 -> %%赠送道具礼包
        GiftPackageID = CommodityConfig#res_goods.gift_id,
        GiftPackageConfig = cache_csv:get_shop_package_config_by_id(GiftPackageID),
        inc_shop_package(Account2, GiftPackageConfig);
      4 -> %%道具
        GiftCount = CommodityConfig#res_goods.gift_count,
        GiftPropertyNo = CommodityConfig#res_goods.gift_id,
        cache_csv:get_property_config(GiftPropertyNo),
        GiftPropertyTree = cache_api:inc_tree_element([{GiftPropertyNo, GiftCount}], Account2#account.backpack#backpack.prop_list),
        GiftAfterNBackpack = Account2#account.backpack#backpack{prop_list = GiftPropertyTree},
        Account2#account{backpack = GiftAfterNBackpack};
      5 ->
        cache_api:inc_strength(Account2, CommodityConfig#res_goods.gift_count, "BuyGoodsGiftStrength");
      OtherType ->
        ?FILE_LOG_INFO("buy goods gift other type [~p]", [OtherType]),
        Account2
    end,

  Shop = Account3#account.shop#shop{goods_list = DailyGoodsList, permanent_buy_list = PermanentGoodsList},

  %%添加流水日志
  cache_log_util:write_consume_log(Account#account.uin, CommodityConfig#res_goods.goods_type, CommodityConfig#res_goods.id, CommodityConfig#res_goods.goods_no, CommodityConfig#res_goods.goods_count, CommodityConfig#res_goods.money_type, CostCount, []),

  %%更新任务和成就
  {UpdateMissionList, Mission1} =cache_mission:update_mission(consumption, Account#account.uin, Account3#account.mission, {CommodityConfig#res_goods.money_type, CommodityConfig#res_goods.goods_type, CostCount}),
  NAccount = Account3#account{mission = Mission1, shop = Shop},
  {NAccount, ["shop", "gold_coin", "gem", "backpack", "achievement"], {UpdateMissionList}}.

add_item_ss(Account, Uin, GoodsId,AddCount, _Ip) when is_record(Account,account) andalso is_integer(Uin)  ->
	ItemBaseInfo = cache_csv:get_item_base_info(GoodsId),
	ItemType = ItemBaseInfo#itemBaseInfo.itemType,
	_ItemID = ItemBaseInfo#itemBaseInfo.id,
	AddCount_int = dd_util:to_integer(AddCount),
	{success, UUid} = cache_guid:alloc_guid(Uin),
	NBackpack =
	case ItemType of
		?TYPE_COMPOSE_MATERIAL->PropertyTree = cache_api:inc_tree_element([{UUid,GoodsId, AddCount_int}], Account#account.backpack#backpack.s_material_list),
  			Account#account.backpack#backpack{s_material_list = PropertyTree};
		?TYPE_PLAYER_PIECE->PropertyTree = cache_api:inc_tree_element([{UUid,GoodsId, AddCount_int}], Account#account.backpack#backpack.s_fragment_list),
  			Account#account.backpack#backpack{s_fragment_list = PropertyTree};
		?TYPE_EQUIP->PropertyTree = cache_api:inc_tree_element([{UUid,GoodsId, AddCount_int}], Account#account.backpack#backpack.s_equipment_list),
  			Account#account.backpack#backpack{s_equipment_list = PropertyTree};
		?TYPE_CONSUME_MATERIAL->PropertyTree = cache_api:inc_tree_element([{UUid,GoodsId, AddCount_int}], Account#account.backpack#backpack.s_consumables_list),
  			Account#account.backpack#backpack{s_consumables_list = PropertyTree};
		_ ->{}
	end,
  	{Account#account{backpack = NBackpack},["backpack"]}.

del_item_ss(Account, Uin, GoodsId,DelCount, _Ip) when is_record(Account,account) andalso is_integer(Uin)  ->
	ItemBaseInfo = cache_csv:get_item_base_info(GoodsId),
	ItemType = ItemBaseInfo#itemBaseInfo.itemType,
	_ItemID = ItemBaseInfo#itemBaseInfo.id,
	DelCount_int = dd_util:to_integer(DelCount),
	{success, UUid} = cache_guid:alloc_guid(Uin),
	NBackpack =
	case ItemType of
		?TYPE_COMPOSE_MATERIAL->PropertyTree = cache_api:dec_tree_element([{UUid,GoodsId, DelCount_int}], Account#account.backpack#backpack.s_material_list),
  			Account#account.backpack#backpack{s_material_list = PropertyTree};
		?TYPE_PLAYER_PIECE->PropertyTree = cache_api:dec_tree_element([{UUid,GoodsId, DelCount_int}], Account#account.backpack#backpack.s_fragment_list),
  			Account#account.backpack#backpack{s_fragment_list = PropertyTree};
		?TYPE_EQUIP->PropertyTree = cache_api:dec_tree_element([{UUid,GoodsId, DelCount_int}], Account#account.backpack#backpack.s_equipment_list),
  			Account#account.backpack#backpack{s_equipment_list = PropertyTree};
		?TYPE_CONSUME_MATERIAL->PropertyTree = cache_api:dec_tree_element([{UUid,GoodsId, DelCount_int}], Account#account.backpack#backpack.s_consumables_list),
  			Account#account.backpack#backpack{s_consumables_list = PropertyTree};
		_ ->{}
	end,
  	{Account#account{backpack = NBackpack},["backpack"]}.


inc_shop_package(Account, Package) when is_record(Account, account) andalso is_record(Package, res_shop_package) ->
  List =
    [
      {Package#res_shop_package.package_1_type, Package#res_shop_package.package_1_id, Package#res_shop_package.package_1_count},
      {Package#res_shop_package.package_2_type, Package#res_shop_package.package_2_id, Package#res_shop_package.package_2_count},
      {Package#res_shop_package.package_3_type, Package#res_shop_package.package_3_id, Package#res_shop_package.package_3_count},
      {Package#res_shop_package.package_4_type, Package#res_shop_package.package_4_id, Package#res_shop_package.package_4_count},
      {Package#res_shop_package.package_5_type, Package#res_shop_package.package_5_id, Package#res_shop_package.package_5_count}
    ],               %%道具1类型（1=金币，2=钻石，3=道具）
  lists:foldl(
    fun({Type, ID, Count}, TmpAccount) ->
      case Type of
        1 ->
          cache_api:account_inc_money(TmpAccount, 2, Count, "BuyGoodsPackageOrGift");
        2 ->
          cache_api:account_inc_money(TmpAccount, 1, Count, "BuyGoodsPackageOrGift");
        3 ->
          if
            Count > 0 ->
              cache_csv:get_property_config(ID),
              PropertyTree = cache_api:inc_tree_element([{ID, Count}], TmpAccount#account.backpack#backpack.prop_list),
              NBackpack = TmpAccount#account.backpack#backpack{prop_list = PropertyTree},
              TmpAccount#account{backpack = NBackpack};
            true ->
              TmpAccount
          end;
        _ ->
          TmpAccount
      end
    end, Account, List).

check_commodity_valid(Account, GoodsID) when is_record(Account, account) andalso is_list(GoodsID) ->
  CommodityConfig = cache_csv:get_commodity_config(GoodsID),
  case CommodityConfig#res_goods.goods_type of
    1 -> %%钻石
      ?FILE_LOG_WARNING("check_commodity_valid => buy goods: bug gem not from this interface", []),
      throw({custom, "HintInvalidRequest"});
    _ -> %%金币或者道具
      RestrictCount = CommodityConfig#res_goods.restrict_count,
      case CommodityConfig#res_goods.refresh_type of
        1 -> %%每日刷新
          if
            RestrictCount > 0 -> %%检查 限购次数
              case cache_util:find_commodity_by_id(Account#account.shop#shop.goods_list, GoodsID) of
                {success, GoodsItem} ->
                  TRemainCount = GoodsItem#goods.remain_count,
                  if
                    TRemainCount > 0 -> ok;
                    true ->
                      ?FILE_LOG_WARNING("buy goods: goods [~p] has no chance!", [GoodsID]),
                      throw({custom, "HintNoPuchaseChance"})
                  end;
                fail ->
                  ?FILE_LOG_WARNING("commodity data error, [~p]", [GoodsID]),
                  ok
              end;
            true -> ok
          end;
        2 -> %%永久刷新
          if
            RestrictCount > 0 -> %%检查 限购次数
              case cache_util:find_commodity_by_id(Account#account.shop#shop.permanent_buy_list, GoodsID) of
                {success, GoodsItem} ->
                  TRemainCount = GoodsItem#goods.remain_count,
                  if
                    TRemainCount > 0 -> ok;
                    true ->
                      ?FILE_LOG_WARNING("buy goods: goods [~p] has no chance!", [GoodsID]),
                      throw({custom, "HintNoPuchaseChance"})
                  end;
                fail ->
                  ?FILE_LOG_WARNING("commodity data error, [~p]", [GoodsID]),
                  ok
              end;
            true -> ok
          end;
        _ -> ok
      end,
      %%检查是否在销售期内
      CurTime = dd_util:timestamp(),
      StartTs =  CommodityConfig#res_goods.start_ts,
      OverTs =  CommodityConfig#res_goods.over_ts,
      if
        StartTs =< 0 -> ok; %%没有限购期
        StartTs > 0 andalso CurTime >= StartTs andalso CurTime =< OverTs -> ok;
        true ->
          ?FILE_LOG_WARNING("commodity not in the sales period.", []),
          throw({custom, "HintNotAllowPurchase"})
      end,
      %%检查消耗的钻石或者金币是否充足
      CostCount = dd_util:to_integer(CommodityConfig#res_goods.money_count),

    CurGem = Account#account.gem,
    CurGoldCoin = Account#account.gold_coin,
    case CommodityConfig#res_goods.money_type of
       1 -> %%钻石
         if
           CurGem >= CostCount -> ok;
           true ->
             ?FILE_LOG_WARNING("buy goods: have not enough gem", []),
             throw({custom, "HintInsufficientDiamond"})
         end;
       2 -> %%金币
        if
          CurGoldCoin >= CostCount -> ok;
          true ->
            ?FILE_LOG_WARNING("buy goods: have not enough gold", []),
            throw({custom, "HintInsufficientGold"})
        end;
       _ ->
        ?FILE_LOG_WARNING("check_commodity_valid => money type error", []),
         throw({custom, "HintSystemDataError"})
    end
  end.


get_login_reward(Account, LoginTimes) when is_record(Account, account) andalso is_integer(LoginTimes) ->
  Id =
    if
      LoginTimes > 7 -> 7;
      true -> LoginTimes
    end,
  RewardConfig = cache_csv:get_login_reward_config(Id),
  NAccount = obtain_reward("LoginReward", Account, RewardConfig#res_login_reward.reward_type, [], RewardConfig#res_login_reward.reward_number),
  LoginList = cache_util:delete_login_reward_by_id(NAccount#account.login_reward#login_reward.login_reward_list, LoginTimes),
  NLoginReward = NAccount#account.login_reward#login_reward{login_reward_list = LoginList},
  NNAccount = NAccount#account{login_reward = NLoginReward},

  %%写日志
  RewardDesc = dd_util:to_list(LoginTimes),
  cache_log_util:write_reward_log(Account#account.uin, "LoginReward", dd_util:timestamp(), RewardConfig#res_login_reward.reward_type,
    [], RewardConfig#res_login_reward.reward_number, RewardDesc),

  {NNAccount, ["gold_coin", "gem", "login_reward"]}.


check_login_reward(LoginReward, LoginTimes) when is_record(LoginReward, login_reward) andalso is_integer(LoginTimes) ->
  if
    LoginTimes =< 0 ->
      throw({custom, "HintRequestDataError"});
    true ->
      ok
  end,
  case cache_util:find_login_reward(LoginReward#login_reward.login_reward_list, LoginTimes) of
    {success, _RewardItem} -> ok;
    fail ->
      ?FILE_LOG_ERROR("get_login_reward fail: reward [~pth]day has been draw or not exist", [LoginTimes]),
      throw({custom, "HintRewardNotAllowDraw"})
  end.


strengthen(Account, Type, ObjectType, ObjectID, TargetID, TowerID, PieceList) when is_record(Account, account) andalso is_list(PieceList) andalso is_integer(Type) andalso is_integer(ObjectType) andalso is_list(ObjectID) andalso is_list(TargetID) andalso is_list(TowerID)->
  case Type of
    0 ->      %%升级
      case ObjectType of
        0 ->           %%装备
          upgrade_equipment(Account, ObjectID, TowerID);
        Other ->
          ?FILE_LOG_ERROR("can not upgrade ~p", [Other]),
          throw({custom, "HintRequestDataError"})
      end;
    1 ->      %%进阶
      case ObjectType of
        0 ->           %%装备
          advanced_equipment(Account, ObjectID, TargetID, TowerID, PieceList);
        Other ->
          ?FILE_LOG_ERROR("can not advanced ~p", [Other]),
          throw({custom, "HintRequestDataError"})
      end;
    _ ->
      ?FILE_LOG_ERROR("error strengthen type [~p]", [Type]),
      throw({custom, "HintRequestDataError"})
  end.

%%升级装备
upgrade_equipment(Account, EquipId, TowerID) when is_record(Account, account) andalso is_list(EquipId) andalso is_list(TowerID) ->
  ?FILE_LOG_DEBUG("upgrade equipment: equipment_id = ~p, tower_id = ~p", [EquipId, TowerID]),
  %%读取装备数据
  {success, EquipItem} =
    case TowerID of
      [] ->     %%背包中的数据
        cache_util:find_equipment_by_id(Account#account.backpack#backpack.equipment_list, EquipId);
      _Value ->
        {success, Tower} = cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, TowerID),
        cache_util:find_equipment_by_id(Tower#character.equipment_list, EquipId)
    end,

  %%装备等级
  MaxLevel = cache_api:get_equipment_max_level(Account),
  EquipLevel = cache_api:get_equipment_level(EquipItem#equipment.exp, MaxLevel),
  EquipUpgradeCost = cache_csv:get_equip_upgrade_cost_by_id(EquipLevel),
  EquipmentConfig = cache_csv:get_equipment_config(EquipItem#equipment.no),
  CostGold =
    case EquipmentConfig#res_equipment.type of
      1 -> %%主武器
        EquipUpgradeCost#res_equip_upgrade_cost.main_cost;
      2 ->
        EquipUpgradeCost#res_equip_upgrade_cost.off_cost;
      3 ->
        EquipUpgradeCost#res_equip_upgrade_cost.head_cost;
      Other ->
        ?FILE_LOG_ERROR("Upgrade Error EquipmentType, type = ~p", [Other]),
        throw({custom, "HintSystemDataError"})
    end,

  %%下一级经验值
  ExpConfig = cache_csv:get_exp_config(EquipLevel),
  EquipExp = ExpConfig#res_exp_config.total_exp,
  %%更新金币数
  ?FILE_LOG_DEBUG("upgrade_equipment[~p]: cost gold = [~p]", [EquipId, CostGold]),
  NAccount = cache_api:account_dec_money(Account, 2, CostGold, "UpgradeEquipmentCost"),

  %%获取玩家所有装备及当前升级装备的信息
  PlayerEquipmentList =
    lists:foldr(
      fun(TowerItem, TmpList) ->
        lists:merge(TmpList, TowerItem#character.equipment_list)
      end, [], Account#account.heros#heros.character_lists),

  StrengthenData = #strengthen_data{type = 0, target_id = EquipItem#equipment.id, target_no = EquipItem#equipment.no, target_level = EquipLevel + 1, equipment_list = PlayerEquipmentList},

  %%流水日志
  cache_log_util:write_strengthen_log(Account#account.uin, 0, EquipItem#equipment.id, EquipItem#equipment.no, CostGold, [], EquipItem#equipment.no, EquipItem#equipment.exp, EquipItem#equipment.no, EquipExp),

  %%更新所有数据
  NEquipmentItem = EquipItem#equipment{exp = EquipExp},
  {success, TowerItem} = cache_util:find_tower_by_id(NAccount#account.heros#heros.character_lists, TowerID),
  NTowerEquipmentList = cache_util:update_equipment(TowerItem#character.equipment_list, NEquipmentItem),
  Herolist = cache_util:update_tower(NAccount#account.heros#heros.character_lists, TowerItem#character{equipment_list = NTowerEquipmentList}),
  NHero = NAccount#account.heros#heros{character_lists = Herolist},
  {NAccount#account{heros = NHero}, ["hero", "gold_coin"], {NEquipmentItem, StrengthenData}}.

%%进阶装备
advanced_equipment(Account, EquipId, TargetEquipID, TowerID, PieceList) when is_record(Account, account) andalso is_list(EquipId) andalso is_list(TowerID) andalso is_list(TargetEquipID) andalso is_list(PieceList) ->
  %%读取装备数据
  {success, EquipItem} =
    case TowerID of
      [] ->     %%背包中的数据
        cache_util:find_equipment_by_id(Account#account.backpack#backpack.equipment_list, EquipId);
      _Value ->
        {success, Tower} = cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, TowerID),
        cache_util:find_equipment_by_id(Tower#character.equipment_list, EquipId)
    end,
  %%更新背包数据
%%   NPieceList = lists:foldl(
%%     fun({ID, Count}, TmpList) ->
%%       {success, PE} = cache_util:find_piece_by_id(TmpList, ID),
%%       NCount =  PE#piece.count - Count,
%%       if
%%         NCount =< 0 -> cache_util:delete_piece_by_id(TmpList, ID);
%%         true -> cache_util:update_piece(TmpList, PE#piece{count = NCount})
%%       end
%%     end, Account#account.backpack#backpack.equip_piece, PieceList),
  NPieceTree = cache_api:dec_tree_element(PieceList, Account#account.backpack#backpack.equip_piece),

  %%计算金币数
  TargetEquipConfig = cache_csv:get_equipment_config(TargetEquipID),
  Cost = TargetEquipConfig#res_equipment.advance_cost,
  NAccount = cache_api:account_dec_money(Account, 2, Cost, "AdvancedEquipmentCost"),

  MaxLevel = cache_api:get_equipment_max_level(Account),
  %%获取强化数据
  PlayerEquipmentList =
    lists:foldr(
      fun(TowerItem, TmpList) ->
        lists:merge(TmpList, TowerItem#character.equipment_list)
      end, Account#account.backpack#backpack.equipment_list,Account#account.heros#heros.character_lists),
  StrengthenData = #strengthen_data{type = 1, target_id = EquipItem#equipment.id, target_no = TargetEquipID, target_level = cache_api:get_equipment_level(EquipItem#equipment.exp, MaxLevel), equipment_list = PlayerEquipmentList},

  %%流水日志
  cache_log_util:write_strengthen_log(Account#account.uin, 1, EquipItem#equipment.id, EquipItem#equipment.no, Cost,
    PieceList, EquipItem#equipment.no, EquipItem#equipment.exp, TargetEquipID, EquipItem#equipment.exp),

  %%更新所有数据
  {success, TowerItem} = cache_util:find_tower_by_id(NAccount#account.heros#heros.character_lists, TowerID),
  NTowerEquipmentList = cache_util:update_equipment(TowerItem#character.equipment_list, EquipItem#equipment{no = TargetEquipID}),
  Herolist = cache_util:update_tower(NAccount#account.heros#heros.character_lists, TowerItem#character{equipment_list = NTowerEquipmentList}),
  NHero = NAccount#account.heros#heros{character_lists = Herolist},
  NBackpack = NAccount#account.backpack#backpack{equip_piece = NPieceTree},
  {NAccount#account{backpack = NBackpack, heros = NHero}, ["backpack", "hero", "gold_coin"], {EquipItem#equipment{no = TargetEquipID},StrengthenData}}.


check_strengthen_data(Account, Type, ObjectType, ObjectID, TargetID, TowerID, MaterialList) when is_record(Account, account) andalso is_list(MaterialList) andalso is_integer(Type) andalso is_integer(ObjectType) andalso is_list(ObjectID) andalso is_list(TargetID) andalso is_list(TowerID)->
  if
    Type =:= 0 ->      %%升级
      case ObjectType of
        0 ->           %%装备
         check_upgrade_equipment(Account, ObjectID, TowerID, MaterialList);
        1 ->           %%技能
          ?FILE_LOG_ERROR("can not upgrade skill", []),
          throw({custom, "HintRequestDataError"});
        2 ->           %%塔
          ?FILE_LOG_ERROR("can not upgrade tower", []),
          throw({custom, "HintRequestDataError"})
      end;
    Type =:= 1 ->      %%进阶
      case ObjectType of
        0 ->           %%装备
          check_advanced_equipment(Account, ObjectID, TargetID, TowerID, MaterialList);
        1 ->           %%技能
          ?FILE_LOG_ERROR("can not anvanced skill", []),
          throw({custom, "HintRequestDataError"});
        2 ->           %%塔
          ?FILE_LOG_ERROR("can not anvanced tower", []),
          throw({custom, "HintRequestDataError"})
      end;
    true ->
      ?FILE_LOG_ERROR("error strengthen type [~p]", [Type]),
      throw({custom, "HintRequestDataError"})
  end.

%%升级
check_upgrade_equipment(Account, EquipId, TowerID, MaterialList) when is_record(Account, account) andalso is_list(EquipId) andalso is_list(TowerID) andalso is_list(MaterialList) ->
 EquipItem =
    case TowerID of
      [] ->     %%背包中的数据
        check_equipment_in_list(EquipId, Account#account.backpack#backpack.equipment_list);
      _Value ->
        case cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, TowerID) of
          {success, Tower} ->
            check_equipment_in_list(EquipId, Tower#character.equipment_list);
          fail ->
            ?FILE_LOG_ERROR("tower not exist [~p]", [TowerID]),
            throw({custom, "HintRequestDataError"})
        end
  end,
  %%检查是否满级
  MaxLevel = cache_api:get_equipment_max_level(Account),
  %%EquipmentConfig = cache_csv:get_equipment_config(EquipItem#equipment.no),
  MaxLevelExp = cache_api:get_total_exp_by_level(MaxLevel - 1),
  if
    MaxLevelExp  =< EquipItem#equipment.exp ->
      ?FILE_LOG_ERROR("equipment has been full grade [~p]", [EquipId]),
      throw({custom, "HintEquipmentFullGrade"});
    true -> ok
  end,

  %%装备等级
  EquipLevel = cache_api:get_equipment_level(EquipItem#equipment.exp, MaxLevel),
  EquipUpgradeCost = cache_csv:get_equip_upgrade_cost_by_id(EquipLevel),
  EquipmentConfig = cache_csv:get_equipment_config(EquipItem#equipment.no),
  CostGold =
    case EquipmentConfig#res_equipment.type of
      1 -> %%主武器
        EquipUpgradeCost#res_equip_upgrade_cost.main_cost;
      2 ->
        EquipUpgradeCost#res_equip_upgrade_cost.off_cost;
      3 ->
        EquipUpgradeCost#res_equip_upgrade_cost.head_cost;
      Other ->
        ?FILE_LOG_ERROR("Upgrade Error EquipmentType, type = ~p", [Other]),
        throw({custom, "HintSystemDataError"})
    end,

  ?FILE_LOG_DEBUG("upgrade equipment ~p, ~p, curlevel = ~p, costgold = ~p", [EquipItem#equipment.id, EquipItem#equipment.no, EquipLevel, CostGold]),
  %%检查金币是否够数（升级需要消耗金币）
  if
    CostGold > Account#account.gold_coin ->
      ?FILE_LOG_ERROR("have not enough money to upgrade", []),
      throw({custom, "HintInsufficientGold"});
    true -> ok
  end.

%%进阶
check_advanced_equipment(Account, EquipId, TargetEquipID, TowerID, PieceList) when is_record(Account, account) andalso is_list(EquipId) andalso is_list(TowerID) andalso is_list(TargetEquipID) andalso is_list(PieceList) ->
  EquipItem =
    case TowerID of
      [] ->     %%背包中的数据
        check_equipment_in_list(EquipId, Account#account.backpack#backpack.equipment_list);
      _Value ->
        case cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, TowerID) of
          {success, Tower} ->
            check_equipment_in_list(EquipId, Tower#character.equipment_list);
          fail ->
            ?FILE_LOG_ERROR("tower not exist [~p]", [TowerID]),
            throw({custom, "HintRequestDataError"})
        end
    end,
  EquipConfig = cache_csv:get_equipment_config(EquipItem#equipment.no),

  %%检查目标装备是否在配置表中
  AdvancedID1 = EquipConfig#res_equipment.upgrade_obj_id1,
  AdvancedID2 = EquipConfig#res_equipment.upgrade_obj_id2,
  if
    TargetEquipID =:= AdvancedID1 -> ok;
    TargetEquipID =:= AdvancedID2 -> ok;
    true ->
      ?FILE_LOG_ERROR("advanced target invalid, [~p, ~p] =/= [~p]", [AdvancedID1, AdvancedID2, TargetEquipID]),
      throw({custom, "HintRequestDataError"})
  end,
  %%检查金币是否够数（升级需要消耗金币）
  TargetEquipmentConfig = cache_csv:get_equipment_config(TargetEquipID),
  if
    TargetEquipmentConfig#res_equipment.advance_cost > Account#account.gold_coin ->
      ?FILE_LOG_ERROR("have not enough money to upgrade", []),
      throw({custom, "HintInsufficientGold"});
    true -> ok
  end,
  %%检查所需碎片是否充足
  check_piece_enough(PieceList, Account#account.backpack#backpack.equip_piece),
  %%检查所需材料是否为配置文件中的所写材料
  NeedList = [{TargetEquipmentConfig#res_equipment.advance_material_1_id, TargetEquipmentConfig#res_equipment.advance_material_1_num},
    {TargetEquipmentConfig#res_equipment.advance_material_2_id, TargetEquipmentConfig#res_equipment.advance_material_2_num},
    {TargetEquipmentConfig#res_equipment.advance_material_3_id, TargetEquipmentConfig#res_equipment.advance_material_3_num},
    {TargetEquipmentConfig#res_equipment.advance_material_4_id, TargetEquipmentConfig#res_equipment.advance_material_4_num},
    {TargetEquipmentConfig#res_equipment.advance_material_5_id, TargetEquipmentConfig#res_equipment.advance_material_5_num}],

  NeedList1 = lists:foldl(
    fun({ID, Count}, TmpList) ->
      if
        ID =/= [] andalso Count > 0 ->
          [{ID, Count} | TmpList];
        true ->
          ?FILE_LOG_WARNING("need list element empty", []),
          TmpList
      end
    end, [], NeedList),

  if
    length(PieceList) =/= length(NeedList1) ->
      ?FILE_LOG_DEBUG("advanced need piece invalid, [~p, ~p]", [PieceList, NeedList1]),
      throw({custom, "HintRequestDataError"});
    true -> ok
  end,

  lists:foreach(
    fun({ID, Count}) ->
      case proplists:get_value(ID, PieceList, undefined) of
        undefined ->
          ?FILE_LOG_ERROR("advanced need piece invalid [~p, ~p]", [ID, Count]),
          throw({custom, "HintRequestDataError"});
        Count -> ok;
        _ ->
          ?FILE_LOG_ERROR("advanced need piece invalid [~p, ~p]", [ID, Count]),
          throw({custom, "HintRequestDataError"})
      end
    end, NeedList1).

check_world_map_block(Stage, WBlockID, GetMaterialList) when is_record(Stage, stage) andalso  is_list(WBlockID) andalso is_list(GetMaterialList) ->
  WorldMapBlockConfig = cache_csv:get_world_map_block_config(WBlockID),
  UnlockTollgateID = WorldMapBlockConfig#res_world_map_block.unlock_tollgate,
  %%关卡类型是否为收成障碍物
  if
    WorldMapBlockConfig#res_world_map_block.interaction_type =:= 2 -> ok;
    true ->
      ?FILE_LOG_ERROR("not block type [~p]", [WBlockID]),
      throw({custom, "HintRequestDataError"})
  end,
  %%是否解锁
  case cache_util:find_finished_tollgate_by_id(Stage, UnlockTollgateID) of
    fail ->
      throw({custom, "HintTollgateUnlock"});
    {success, _} -> ok
  end,
  %%是否还有采集次数
  case cache_util:find_world_map_block(Stage#stage.harvest_obstacles_list, WBlockID) of
     {success, WBItem} ->
        if
          WBItem#harvest_obstacles_item.remain_number =< 0 ->
            ?FILE_LOG_ERROR("map block have no times today [~p]", [WBlockID]),
            throw({custom, "HintTollgateNoTimes"});
          true -> ok
        end;
     fail ->
        ok   %%列表中不存在，则默认有INIT_BLOCL_TIMES
  end,
  %%检查掉落物品
  lists:map(
    fun({ID, _Count}) ->
      case lists:member(ID, WorldMapBlockConfig#res_world_map_block.materials) of
        true -> ok;
        false ->
          ?FILE_LOG_ERROR("world map block drop not invalid [~p]", [ID]),
          throw({custom, "HintRequestDataError"})
      end
    end, GetMaterialList).

%% test_add_equipment_material(Uin, Backpack, AddList) when is_integer(Uin) andalso is_record(Backpack, backpack) andalso is_list(AddList) ->
%%   lists:foldr(
%%     fun({ID, Count}, TempNB) ->
%%       DropConfig = cache_csv:get_drop_config(ID),
%%       update_backpack_by_drop(Uin, DropConfig, TempNB, Count)
%%     end, Backpack, AddList).

check_equipment_in_list(EquipmentID, EquipmentList) when is_list(EquipmentID) andalso is_list(EquipmentList) ->
  case cache_util:find_equipment_by_id(EquipmentList, EquipmentID) of
    {success, Value} -> Value;
    fail ->
      ?FILE_LOG_ERROR("equipment not exist [~p]", [EquipmentID]),
      throw({custom, "HintRequestDataError"})
  end.

check_piece_enough(NeedPieceList, HavePieceTree) when is_tuple(HavePieceTree) andalso is_list(NeedPieceList) ->
  lists:foreach(
    fun({ID, Count}) ->
      case gb_trees:lookup(ID, HavePieceTree) of
        none -> throw({custom, "HintInsufficientMaterial"});
        {value, Num} ->
          if
            Num < Count -> throw({custom, "HintInsufficientMaterial"});
            true -> ok
          end
      end
    end, NeedPieceList).

get_treasure_item_list(Account, HasGetTreasureDropList, InitTreasureItemList) when is_record(Account, account) andalso is_list(InitTreasureItemList) ->
  UserInscriptionClassTree = get_user_inscription_class(Account),
  get_treasure_item_list_1(InitTreasureItemList, {Account, HasGetTreasureDropList, UserInscriptionClassTree}, {0, 0, []}).
get_treasure_item_list_1([], _, {_CurProb, TotalProb, OutList}) -> {lists:reverse(OutList), TotalProb};
get_treasure_item_list_1([Item | T], {Account, HasGetTreasureDropList, UserInscriptionClassTree}, {CurProb, TotalProb, OutList}) ->
  case check_treasure_item_valid({Account, HasGetTreasureDropList, UserInscriptionClassTree}, Item) of
    true ->
      NItem = Item#res_treasure_item_config{prob_low_value = CurProb, prob_up_value = CurProb + Item#res_treasure_item_config.item_probability - 1},
      get_treasure_item_list_1(T, {Account, HasGetTreasureDropList, UserInscriptionClassTree}, {CurProb + Item#res_treasure_item_config.item_probability, TotalProb + Item#res_treasure_item_config.item_probability, [NItem | OutList]});
    false ->
      get_treasure_item_list_1(T, {Account, HasGetTreasureDropList, UserInscriptionClassTree}, {CurProb, TotalProb, OutList})
  end.

check_treasure_item_valid({Account, HasGetTreasureDropList, UserInscriptionClassTree}, TreasureItem) when is_record(Account, account) andalso is_record(TreasureItem, res_treasure_item_config) ->
  case TreasureItem#res_treasure_item_config.item_type of
    1 -> %%角色
      F = fun(DropItem, DropID) -> DropItem#treasure_item.drop_id =:= DropID end,
      case cache_util:find_item_by_id(HasGetTreasureDropList, F, TreasureItem#res_treasure_item_config.item_id) of
        {success, _} -> false;
        fail ->
          case cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, TreasureItem#res_treasure_item_config.item_id) of
            {success, _} -> false;
            fail -> true
          end
      end;
    4 -> %%铭文
      %%检查铭文是否已经存在(存在则过滤，不存在则检查铭文对应的塔是否存在， 若塔不存在，则消失，若塔存在，则保留)
      F = fun(DropItem, DropID) -> DropItem#treasure_item.drop_id =:= DropID end,
      case cache_util:find_item_by_id(HasGetTreasureDropList, F, TreasureItem#res_treasure_item_config.item_id) of
        {success, _} -> false;
        fail ->
          InscriptionConfig = cache_csv:get_inscription_by_id(TreasureItem#res_treasure_item_config.item_id),
          case gb_trees:lookup(InscriptionConfig#res_inscription.inscription_class, UserInscriptionClassTree) of
            none ->
              %%检查铭文所对应的塔是否存在
              InscriptionConfig = cache_csv:get_inscription_by_id(TreasureItem#res_treasure_item_config.item_id),
              case InscriptionConfig#res_inscription.belong_tower of
                "TY" -> true; %%通用
                TowerID ->
                  case cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, TowerID) of
                    {success, _} -> true;
                    fail -> false
                  end
              end;
            {value, _} -> false
          end
      end;
    5 -> %%铭文碎片
      InscriptionPieceConfig = cache_csv:get_inscription_piece_config_by_id(TreasureItem#res_treasure_item_config.item_id),
      case InscriptionPieceConfig#res_inscription_piece.belong_tower of
        "" -> true;
        TowerID ->
          case cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, TowerID) of
            {success, _} -> true;
            fail -> false
          end
      end;
    _ -> true %%1 角色 2 装备碎片 3 金币
  end.

update_player_data_game_end(Account, Gold, TollgateID) ->
  if
    TollgateID < 10000 ->
      update_player_data_game_end_1(tollgate_mode, Account, Gold, TollgateID);
    TollgateID < 20000 ->
      update_player_data_game_end_1(endless_mode, Account, Gold, TollgateID);
    TollgateID < 30000 ->
      update_player_data_game_end_1(activity_mode, Account, Gold, TollgateID);
    true -> Account
  end.

update_player_data_game_end_1(tollgate_mode, Account, _, TollgateID) when is_record(Account, account) andalso is_integer(TollgateID) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  GainGold = TollgateConfig#res_stage.gain_gold,
  CostStrength = get_tollgate_energy_cost(TollgateID, Account#account.stage),
  case TollgateConfig#res_stage.type of
    1 ->
      NAccount = cache_api:account_inc_money(Account, 2, GainGold, "TollgateMode_Normal_GameEndGet"),
      cache_api:dec_strength(NAccount, CostStrength, "TollgateMode_Normal_Cost");
    2 ->
      NAccount = cache_api:account_inc_money(Account, 2, GainGold, "TollgateMode_Elite_GameEndGet"),
      cache_api:dec_strength(NAccount, CostStrength, "TollgateMode_Elite_Cost");
    3 ->
      NAccount = cache_api:account_inc_money(Account, 2, GainGold, "TollgateMode_Boss_GameEndGet"),
      cache_api:dec_strength(NAccount, CostStrength, "TollgateMode_Boss_Cost")
  end;
update_player_data_game_end_1(endless_mode, Account, _, TollgateID) when is_record(Account, account) andalso is_integer(TollgateID) ->
  CostStrength = get_tollgate_energy_cost(TollgateID, Account#account.stage),
  cache_api:dec_strength(Account, CostStrength, "EndlessMode_Cost");
update_player_data_game_end_1(activity_mode, Account, _, TollgateID) when is_record(Account, account) andalso is_integer(TollgateID) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  GainGold = TollgateConfig#res_stage.gain_gold,
  CostStrength = get_tollgate_energy_cost(TollgateID, Account#account.stage),
  case TollgateConfig#res_stage.type of
    1 ->
      NAccount = cache_api:account_inc_money(Account, 2, GainGold, "ActivityMode_Normal_GameEndGet"),
      cache_api:dec_strength(NAccount, CostStrength, "ActivityMode_Normal_Cost");
    2 ->
      NAccount = cache_api:account_inc_money(Account, 2, GainGold, "ActivityMode_Elite_GameEndGet"),
      cache_api:dec_strength(NAccount, CostStrength, "ActivityMode_Elite_Cost")
  end.

update_backpack_with_props_drops(Account, UsePropsList, DropsList) when is_record(Account, account) andalso is_list(UsePropsList) andalso is_list(DropsList)  ->
  PropTree = Account#account.backpack#backpack.prop_list,
  NewPropTree = cache_api:dec_tree_element(UsePropsList, PropTree),

  NBackpack = Account#account.backpack#backpack{prop_list = NewPropTree},
  %%更新掉落
  DropItemList =
    lists:foldl(
      fun({ID, Count}, TmpL) ->
        List = lists:foldl(
          fun(_Index, TmpItemList) ->
            DropNConfig = cache_csv:get_tollgate_drop_by_id(ID),
            ChestConfig = cache_csv:get_treasure_config_by_id(DropNConfig#res_tollgate_drop.treasure_id),
            {TreasureItemList, TotalProb} = get_treasure_item_list(Account, TmpItemList, ChestConfig#res_treasure_config.item_list),
            if
              TotalProb =< 0 -> throw({custom, "HintSystemDataError"});
              true -> ok
            end,
            RandValue = dd_util:random_in_range(?MAX_SEED) rem TotalProb,
            {success, Item} = compare_treasure_prob(TreasureItemList, RandValue),
            DropItem = #treasure_item{drop_id = Item#res_treasure_item_config.item_id, sub_id = Item#res_treasure_item_config.sub_drop_id,
            treasure_id = ID, drop_type = Item#res_treasure_item_config.item_type, count = Item#res_treasure_item_config.item_count, drop_level = Item#res_treasure_item_config.item_level},
            [DropItem | TmpItemList]
          end, [], lists:seq(1, Count)),
        lists:merge(List, TmpL)
      end, [], DropsList),
  ?FILE_LOG_DEBUG("normal drop treasure list = ~p", [DropItemList]),
  NAccount = Account#account{backpack = NBackpack},
  NewAccount = update_account_with_treasure_list(DropItemList, <<"TollgateDrop">>, NAccount),
  {NewAccount, DropItemList}.

update_endless_drops_props(Account, GameEndData) when is_record(Account, account) andalso is_record(GameEndData,game_end) ->
  Backpack = Account#account.backpack,
  %%更新道具
  PropTree = Backpack#backpack.prop_list,
  NewPropTree = cache_api:dec_tree_element(GameEndData#game_end.use_props, PropTree),
  NBackpack = Backpack#backpack{prop_list = NewPropTree},
  %%更新掉落
  DropItemList =
    lists:foldl(
      fun({ID, Count}, TmpL) ->
        List = lists:foldl(
          fun(_Index, TmpItemList) ->
            ChestConfig = cache_csv:get_treasure_config_by_id(ID),
            {TreasureItemList, TotalProb} = get_treasure_item_list(Account, TmpItemList, ChestConfig#res_treasure_config.item_list),
            if
              TotalProb =< 0 -> throw({custom, "HintSystemDataError"});
              true -> ok
            end,
            RandValue = dd_util:random_in_range(?MAX_SEED) rem TotalProb,
            {success, Item} = compare_treasure_prob(TreasureItemList, RandValue),
            DropItem = #treasure_item{drop_id = Item#res_treasure_item_config.item_id, sub_id = Item#res_treasure_item_config.sub_drop_id,
                      treasure_id = ID, drop_type = Item#res_treasure_item_config.item_type, count = Item#res_treasure_item_config.item_count, drop_level = Item#res_treasure_item_config.item_level},
            [DropItem | TmpItemList]
        end, [], lists:seq(1, Count)),
        lists:merge(List, TmpL)
      end, [], GameEndData#game_end.gain_drops),
  ?FILE_LOG_DEBUG("ENDLESS drop treasure list = ~p", [DropItemList]),
  NAccount = Account#account{backpack = NBackpack},
  NewAccount = update_account_with_treasure_list(DropItemList, <<"EndlessDrop">>, NAccount),
  {NewAccount, DropItemList}.

update_account_with_treasure_list(TreasureList, Reason, Account) when is_record(Account, account) andalso is_list(TreasureList) ->
  update_account_with_treasure_list_1(TreasureList, Reason, Account).
update_account_with_treasure_list_1([], _, Account) -> Account;
update_account_with_treasure_list_1([Item | T], Reason, Account) ->
  NAccount = update_account_with_treasure(Reason, Account, Item),
  update_account_with_treasure_list_1(T, Reason, NAccount).

update_account_with_treasure(<<"TollgateDrop">>, Account, Item) when is_record(Account, account) andalso is_record(Item, treasure_item) ->
  case Item#treasure_item.drop_type of
    1 -> %%角色
      ?FILE_LOG_ERROR("endless drop type error, equip", []),
      throw({custom, "HintRequestDataError"});
    2 -> %%装备进阶碎片
      EquipPieceTree = cache_api:inc_tree_element([{Item#treasure_item.drop_id, Item#treasure_item.count}], Account#account.backpack#backpack.equip_piece),
      TNB = Account#account.backpack#backpack{equip_piece = EquipPieceTree},
      %%stage
      List = [Item | Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.endless_drop],
      NEndDrop = Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop{endless_drop = List},
      NEndless = Account#account.stage#stage.endless_tollgate#endless_tollgate{endless_drop = NEndDrop},
      NStage = Account#account.stage#stage{endless_tollgate = NEndless},
      Account#account{backpack = TNB, stage = NStage};
    3 -> %%金币
      TAccount = cache_api:account_inc_money(Account, 2, Item#treasure_item.count, "EndlessChestGet"),
      List = [Item | Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.endless_drop],
      NEndDrop = Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop{endless_drop = List},
      NEndless = Account#account.stage#stage.endless_tollgate#endless_tollgate{endless_drop = NEndDrop},
      NStage = Account#account.stage#stage{endless_tollgate = NEndless},
      TAccount#account{stage = NStage};
    5 -> %%铭文碎片
      InscriptionPieceTree = cache_api:inc_tree_element([{Item#treasure_item.drop_id, Item#treasure_item.count}], Account#account.backpack#backpack.inscription_piece_list),
      TNB = Account#account.backpack#backpack{inscription_piece_list = InscriptionPieceTree},
      %%stage
      List = [Item | Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.endless_drop],
      NEndDrop = Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop{endless_drop = List},
      NEndless = Account#account.stage#stage.endless_tollgate#endless_tollgate{endless_drop = NEndDrop},
      NStage = Account#account.stage#stage{endless_tollgate = NEndless},
      Account#account{backpack = TNB, stage = NStage};
    Other ->
      ?FILE_LOG_ERROR("endless drop type error, Other = ~p", [Other]),
      throw({custom, "HintRequestDataError"})
  end;
update_account_with_treasure(<<"EndlessDrop">>, Account, Item) when is_record(Account, account) andalso is_record(Item, treasure_item) ->
  case Item#treasure_item.drop_type of
    1 -> %%角色
      ?FILE_LOG_ERROR("endless drop type error, equip", []),
      throw({custom, "HintRequestDataError"});
%%       EL = lists:map(fun(_Index) -> {Item#treasure_item.drop_id, Item#treasure_item.drop_level} end, lists:seq(1, Item#treasure_item.count)),
%%       EquipmentList = cache_api:add_equipment(EL, Account#account.uin, Account#account.backpack#backpack.equipment_list),
%%       TNB = Account#account.backpack#backpack{equipment_list = EquipmentList},
%%       %%stage
%%       List = [Item | Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.endless_drop],
%%       NEndDrop = Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop{endless_drop = List},
%%       NEndless = Account#account.stage#stage.endless_tollgate#endless_tollgate{endless_drop = NEndDrop},
%%       NStage = Account#account.stage#stage{endless_tollgate = NEndless},
%%       Account#account{backpack = TNB, stage = NStage};
    2 -> %%装备进阶碎片
      EquipPieceTree = cache_api:inc_tree_element([{Item#treasure_item.drop_id, Item#treasure_item.count}], Account#account.backpack#backpack.equip_piece),
      TNB = Account#account.backpack#backpack{equip_piece = EquipPieceTree},
      %%stage
      List = [Item | Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.endless_drop],
      NEndDrop = Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop{endless_drop = List},
      NEndless = Account#account.stage#stage.endless_tollgate#endless_tollgate{endless_drop = NEndDrop},
      NStage = Account#account.stage#stage{endless_tollgate = NEndless},
      Account#account{backpack = TNB, stage = NStage};
    3 -> %%金币
      TAccount = cache_api:account_inc_money(Account, 2, Item#treasure_item.count, "EndlessChestGet"),
      List = [Item | Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.endless_drop],
      NEndDrop = Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop{endless_drop = List},
      NEndless = Account#account.stage#stage.endless_tollgate#endless_tollgate{endless_drop = NEndDrop},
      NStage = Account#account.stage#stage{endless_tollgate = NEndless},
      TAccount#account{stage = NStage};
    5 -> %%铭文碎片
      InscriptionPieceTree = cache_api:inc_tree_element([{Item#treasure_item.drop_id, Item#treasure_item.count}], Account#account.backpack#backpack.inscription_piece_list),
      TNB = Account#account.backpack#backpack{inscription_piece_list = InscriptionPieceTree},
      %%stage
      List = [Item | Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.endless_drop],
      NEndDrop = Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop{endless_drop = List},
      NEndless = Account#account.stage#stage.endless_tollgate#endless_tollgate{endless_drop = NEndDrop},
      NStage = Account#account.stage#stage{endless_tollgate = NEndless},
      Account#account{backpack = TNB, stage = NStage};
    Other ->
      ?FILE_LOG_ERROR("endless drop type error, Other = ~p", [Other]),
      throw({custom, "HintRequestDataError"})
  end;
update_account_with_treasure(<<"Lottery">>, Account, Item)  when is_record(Account, account) andalso is_record(Item, treasure_item) ->
  case Item#treasure_item.drop_type of
    1 -> %%角色
      Character = cache_account_def:create_tower_by_id(Account#account.uin, Item#treasure_item.drop_id),
      fail = cache_util:find_tower_by_id(Account#account.heros#heros.character_lists, Item#treasure_item.drop_id),
      Hero = Account#account.heros#heros{character_lists = [Character | Account#account.heros#heros.character_lists]},
      Account#account{heros = Hero};
    2 -> %%装备进阶碎片
      EquipPieceTree = cache_api:inc_tree_element([{Item#treasure_item.drop_id, Item#treasure_item.count}], Account#account.backpack#backpack.equip_piece),
      TNB = Account#account.backpack#backpack{equip_piece = EquipPieceTree},
      %%lottery
      NLotteryList = [Item | Account#account.lottery#lottery.today_lottery_lists],
      NLottery = Account#account.lottery#lottery{last_lottery_ts = dd_util:timestamp(), today_lottery_lists = NLotteryList},
      Account#account{backpack = TNB, lottery = NLottery};
    3 -> %%金币
      TAccount = cache_api:account_inc_money(Account, 2, Item#treasure_item.count, "LotteryGet"),
      %%lottery
      NLotteryList = [Item | TAccount#account.lottery#lottery.today_lottery_lists],
      NLottery = TAccount#account.lottery#lottery{last_lottery_ts = dd_util:timestamp(), today_lottery_lists = NLotteryList},
      TAccount#account{lottery = NLottery};
    4 -> %%铭文
      InscriptionConfig = cache_csv:get_inscription_by_id(Item#treasure_item.drop_id),
      Inscription = #inscription{id = Item#treasure_item.drop_id, pos = InscriptionConfig#res_inscription.type},
      TNB = Account#account.backpack#backpack{inscription_list = [Inscription | Account#account.backpack#backpack.inscription_list]},
      %%lottery
      NLotteryList = [Item | Account#account.lottery#lottery.today_lottery_lists],
      NLottery = Account#account.lottery#lottery{last_lottery_ts = dd_util:timestamp(), today_lottery_lists = NLotteryList},
      Account#account{backpack = TNB, lottery = NLottery};
    5 -> %%铭文碎片
      InscriptionPieceTree = cache_api:inc_tree_element([{Item#treasure_item.drop_id, Item#treasure_item.count}], Account#account.backpack#backpack.inscription_piece_list),
      TNB = Account#account.backpack#backpack{inscription_piece_list = InscriptionPieceTree},
      %%lottery
      NLotteryList = [Item | Account#account.lottery#lottery.today_lottery_lists],
      NLottery = Account#account.lottery#lottery{last_lottery_ts = dd_util:timestamp(), today_lottery_lists = NLotteryList},
      Account#account{backpack = TNB, lottery = NLottery};
    Other ->
      ?FILE_LOG_ERROR("endless drop type error, Other = ~p", [Other]),
      throw({custom, "HintRequestDataError"})
  end.

compare_treasure_prob([], _RandV) -> fail;
compare_treasure_prob([Item | T], RandV) ->
  if
    RandV >= Item#res_treasure_item_config.prob_low_value andalso RandV =< Item#res_treasure_item_config.prob_up_value -> {success, Item};
    true -> compare_treasure_prob(T, RandV)
  end.

update_backpack_by_drop(Uin, DropConfig, Backpack, Count) when is_integer(Uin) andalso is_record(DropConfig, res_drop_config) andalso is_record(Backpack, backpack) andalso is_integer(Count) andalso  Count > 0 ->
  case DropConfig#res_drop_config.type of
    1 -> %%装备
      ?FILE_LOG_ERROR("update_backpack_by_drop => error type : equipment !", []),
      throw({custom, "HintRequestDataError"});
    3 ->
      %%材料
      ?FILE_LOG_ERROR("update_backpack_by_drop => error type : material !", []),
      throw({custom, "HintRequestDataError"});
    4 ->
      %%道具
      PropertyTree = cache_api:inc_tree_element([{DropConfig#res_drop_config.id, Count}], Backpack#backpack.prop_list),
      Backpack#backpack{prop_list = PropertyTree};
    5 ->
      %%装备进阶材料
      EquipPieceTree = cache_api:inc_tree_element([{DropConfig#res_drop_config.id, Count}], Backpack#backpack.equip_piece),
      Backpack#backpack{equip_piece = EquipPieceTree};
    6 ->
      %%铭文碎片
      InscriptionPieceTree = cache_api:inc_tree_element([{DropConfig#res_drop_config.id, Count}], Backpack#backpack.inscription_piece_list),
      Backpack#backpack{equip_piece = InscriptionPieceTree};
    Other ->
      ?FILE_LOG_ERROR("error drop type [type = ~p]", [Other]),
      throw({custom, "HintRequestDataError"})
  end.

update_tollgate_star(Tollgate, GainStar, GainStarSeq) when is_record(Tollgate, tollgate) andalso is_integer(GainStar) andalso is_list(GainStarSeq) andalso GainStar =< 3 ->
  UpdateStarSeq =
    lists:map(
      fun(Index) ->
        case lists:nth(Index, GainStarSeq) * lists:nth(Index, Tollgate#tollgate.max_star_seq) of
          0 -> 0;
          _ -> -1
        end
      end, lists:seq(1, 3)),
  StarCount = 3 - abs(lists:sum(UpdateStarSeq)),
  if
    StarCount =< 3 andalso StarCount > Tollgate#tollgate.max_star -> Tollgate#tollgate{max_star = StarCount, max_star_seq = UpdateStarSeq};
    true -> Tollgate
  end;
update_tollgate_star(Tollgate, _, _) -> Tollgate.

update_tollgate_remain_times(game_end, Tollgate, Count) when is_record(Tollgate, tollgate) andalso Count > 0->
  DailyRemain = Tollgate#tollgate.daily_remain_times,
  DailyAdditionRemain = Tollgate#tollgate.addition_remain_times,
  if
    DailyRemain > 0 -> Tollgate#tollgate{daily_remain_times = DailyRemain - Count};
    DailyAdditionRemain > 0 -> Tollgate#tollgate{addition_remain_times = DailyAdditionRemain - Count};
    true -> throw({custom, "HintTollgateNoTimes"})
  end;
update_tollgate_remain_times(sweep, Tollgate, Count) when is_record(Tollgate, tollgate) andalso Count > 0->
  DailyRemain = Tollgate#tollgate.daily_remain_times,
  DailyAdditionRemain = Tollgate#tollgate.addition_remain_times,
  if
    DailyRemain > 0 -> Tollgate#tollgate{daily_remain_times = DailyRemain - Count};
    DailyAdditionRemain > 0 -> Tollgate#tollgate{addition_remain_times = DailyAdditionRemain - Count};
    true -> throw({custom, "HintTollgateNoTimes"})
  end.

update_tollgate_score(Uin, Tollgate, Score) ->
  {success, RankNode} = dd_config:get_cfg(ranking_node),
  Ret = rpc:call(RankNode, ranking_tollgate, update_tollgate_score, [Uin, Tollgate, Score]),
  ?FILE_LOG_DEBUG("update_tollgate_score, ret  = ~p", [Ret]).

update_endless_score(Uin, Score) ->
  {success, RankNode} = dd_config:get_cfg(ranking_node),
  Ret = rpc:call(RankNode, ranking_tollgate, update_endless_score, [Uin, Score]),
  ?FILE_LOG_DEBUG("update_endless_score, ret  = ~p", [Ret]).

%%成功
update_stage_with_game_end_data(0, Uin, Stage, TollgateID, GainStar, GainStarSeq, GainScore, EndlessCount) when is_record(Stage, stage) andalso is_integer(TollgateID)  andalso is_list(GainStarSeq) andalso is_integer(GainStar) andalso is_integer(GainScore) andalso is_integer(EndlessCount)->
  if
    TollgateID < 10000 ->
      update_stage_with_game_end_data_1(success, tollgate_mode, Uin, Stage, TollgateID, GainStar, GainStarSeq, GainScore, EndlessCount);
    TollgateID < 20000 ->   %%无尽模式
      update_stage_with_game_end_data_1(success, endless_mode, Uin, Stage, TollgateID, GainStar, GainStarSeq, GainScore, EndlessCount);
    TollgateID < 30000 ->  %%活动关卡
      update_stage_with_game_end_data_1(success, activity_mode, Uin, Stage, TollgateID, GainStar, GainStarSeq, GainScore, EndlessCount);
    true ->
      throw({custom, "HintSystemDataError"})
  end;
%%失败
update_stage_with_game_end_data(0, Uin, Stage, TollgateID, GainStar, GainStarSeq, GainScore, EndlessCount) when is_record(Stage, stage) andalso is_integer(TollgateID) andalso is_integer(GainScore) andalso GainScore >= 0->
  if
    TollgateID < 10000 ->
      update_stage_with_game_end_data_1(fail, tollgate_mode,  Uin, Stage, TollgateID, GainStar, GainStarSeq, GainScore, EndlessCount);
    TollgateID < 20000 ->   %%无尽模式
      update_stage_with_game_end_data_1(fail, endless_mode,  Uin, Stage, TollgateID, GainStar, GainStarSeq, GainScore, EndlessCount);
    TollgateID < 30000 ->  %%活动关卡
      update_stage_with_game_end_data_1(fail, activity_mode,  Uin, Stage, TollgateID, GainStar, GainStarSeq, GainScore, EndlessCount);
    true ->
      throw({custom, "HintSystemDataError"})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%更新关卡相关数据
%%闯关模式关卡： 成功
update_stage_with_game_end_data_1(success, tollgate_mode, Uin, Stage, TollgateID, GainStar, GainStarSeq, GainScore, _EndlessCount) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  update_tollgate_score(Uin, TollgateID, GainScore),
  case cache_util:find_finished_tollgate_by_id(Stage, TollgateID) of
    {success, Item} ->
      NewItem = update_tollgate_star(Item, GainStar, GainStarSeq),
      NewItem1 =
        if
          GainScore > NewItem#tollgate.max_score -> NewItem#tollgate{max_score = GainScore};
          true -> NewItem
        end,
      NItem = update_tollgate_remain_times(game_end, NewItem1, 1),
      NItem1 = NItem#tollgate{last_pass_ts = dd_util:timestamp(), cool_time = TollgateConfig#res_stage.cool_time},
      {cache_util:update_tollgate(Stage, NItem1), NItem1};
    fail ->
      Item = #tollgate{id = TollgateID, max_score = GainScore, max_star = GainStar, max_star_seq = GainStarSeq,
        daily_remain_times = TollgateConfig#res_stage.daily_restrict_count - 1, addition_remain_times = TollgateConfig#res_stage.daily_addition_count, last_pass_ts = dd_util:timestamp()},
      {Stage#stage{base_tollgate_list = [Item | Stage#stage.base_tollgate_list]}, Item}
  end;
update_stage_with_game_end_data_1(fail, tollgate_mode, Uin, Stage, TollgateID, _GainStar, _GainStarSeq, GainScore, _EndlessCount) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  update_tollgate_score(Uin, TollgateID, GainScore),
  case cache_util:find_finished_tollgate_by_id(Stage, TollgateID) of
    {success, Item} ->
      NItem =
        if
          GainScore > Item#tollgate.max_score -> Item#tollgate{max_score = GainScore};
          true -> Item
        end,
      {cache_util:update_tollgate(Stage, NItem), NItem};
    fail ->
      NItem = #tollgate{id = TollgateID, max_score = GainScore, max_star = 0, max_star_seq = [-1, -1, -1], last_pass_ts = 0,
        daily_remain_times = TollgateConfig#res_stage.daily_restrict_count, addition_remain_times = TollgateConfig#res_stage.daily_addition_count, cool_time = TollgateConfig#res_stage.cool_time},
      {Stage#stage{base_tollgate_list = [NItem | Stage#stage.base_tollgate_list]}, NItem}
  end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%活动关卡模式 ： 成功
update_stage_with_game_end_data_1(success, activity_mode, Uin, Stage, TollgateID, GainStar, GainStarSeq, GainScore, _EndlessCount) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  update_tollgate_score(Uin, TollgateID, GainScore),
  case cache_util:find_activity_tollgate_by_id(Stage#stage.ac_tollgate, TollgateID) of
    {success, Item} ->
      Score =
        if
          GainScore > Item#activity_tollgate_item.max_score -> GainScore;
          true -> Item#activity_tollgate_item.max_score
        end,
      {Star, StarSeq} =
        if
          GainStar > Item#activity_tollgate_item.max_star -> {GainStar, GainStarSeq};
          true -> {Item#activity_tollgate_item.max_star, Item#activity_tollgate_item.max_star_seq}
        end,
      ActivityRemainTimes = Item#activity_tollgate_item.daily_remain_times - 1,
      NActivityItem = Item#activity_tollgate_item{max_star = Star, max_score = Score, max_star_seq = StarSeq, daily_remain_times  = ActivityRemainTimes, last_finish_ts = dd_util:timestamp(), cool_time = TollgateConfig#res_stage.cool_time},
      List = cache_util:update_activity_tollgate(Stage#stage.ac_tollgate, NActivityItem),
      {Stage#stage{ac_tollgate = List}, NActivityItem};
    fail ->
      NItem = #activity_tollgate_item{tollgate_id = TollgateID, max_score = GainScore, max_star = GainStar, max_star_seq = GainStarSeq, daily_remain_times = TollgateConfig#res_stage.daily_restrict_count - 1, last_finish_ts = dd_util:timestamp(),cool_time = TollgateConfig#res_stage.cool_time},
      NList = [NItem | Stage#stage.ac_tollgate],
      {Stage#stage{ac_tollgate = NList}, NItem}
  end;
update_stage_with_game_end_data_1(fail, activity_mode, Uin, Stage, TollgateID, _GainStar, _GainStarSeq, GainScore, _EndlessCount) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  update_tollgate_score(Uin, TollgateID, GainScore),
  case cache_util:find_activity_tollgate_by_id(Stage#stage.ac_tollgate, TollgateID) of
    {success, Item} ->
      Score =
        if
          GainScore > Item#activity_tollgate_item.max_score -> GainScore;
          true -> Item#activity_tollgate_item.max_score
        end,
      NActivityItem = Item#activity_tollgate_item{max_score = Score},
      List = cache_util:update_activity_tollgate(Stage#stage.ac_tollgate, NActivityItem),
      {Stage#stage{ac_tollgate = List}, NActivityItem};
    fail ->
      NItem = #activity_tollgate_item{tollgate_id = TollgateID, max_score = GainScore, max_star = 0, max_star_seq = [-1, -1, -1], daily_remain_times = TollgateConfig#res_stage.daily_restrict_count, last_finish_ts = 0,cool_time = TollgateConfig#res_stage.cool_time},
      NList = [NItem | Stage#stage.ac_tollgate],
      {Stage#stage{ac_tollgate = NList}, NItem}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%无尽模式
update_stage_with_game_end_data_1(success, endless_mode, Uin, Stage, TollgateID, _GainStar, _GainStarSeq, GainScore, EndlessCount) ->
%%  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  EndlessTollgate = Stage#stage.endless_tollgate,
  update_endless_score(Uin, GainScore),
  CurTime = dd_util:timestamp(),
  NRecord = [#endless_item{time = CurTime, score = GainScore, tollgate_id = TollgateID} | EndlessTollgate#endless_tollgate.week_score_record],
  NEndLess =
    if
      GainScore >  EndlessTollgate#endless_tollgate.max_score ->
        EndlessTollgate#endless_tollgate{max_score = GainScore, max_score_gain_ts = CurTime, this_week_max_score = GainScore, this_week_max_score_gain_ts = CurTime, week_score_record = NRecord};
      true ->
        if
          GainScore > EndlessTollgate#endless_tollgate.this_week_max_score ->
            EndlessTollgate#endless_tollgate{this_week_max_score = GainScore, this_week_max_score_gain_ts = CurTime, week_score_record = NRecord};
          true ->
            EndlessTollgate#endless_tollgate{week_score_record = NRecord}
        end
    end,
  NNEndless =
    if
      EndlessCount > NEndLess#endless_tollgate.max_wave_count ->
        NEndLess#endless_tollgate{max_wave_count = EndlessCount, last_update_ts = dd_util:timestamp()};
      true ->
        NEndLess#endless_tollgate{last_update_ts = dd_util:timestamp()}
    end,
  {Stage#stage{endless_tollgate = NNEndless}, NNEndless}.


calculate_drops(Stage, TollgateID, BossCount) when is_record(Stage, stage) andalso is_integer(TollgateID) andalso is_integer(BossCount) ->
  if
    TollgateID < 10000 orelse TollgateID > 20000 ->
      %%根据关卡开始之前的种子算三个掉落
      TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
      TollgateDropList =
        [
          {TollgateConfig#res_stage.drop_1, TollgateConfig#res_stage.drop_1_prob},
          {TollgateConfig#res_stage.drop_2, TollgateConfig#res_stage.drop_2_prob},
          {TollgateConfig#res_stage.drop_3, TollgateConfig#res_stage.drop_3_prob}
        ],
      TollgateDrop = Stage#stage.tollgate_drop,
      CalDropList = gen_tollgate_drop(TollgateDropList, TollgateDrop#tollgate_drop.seed_val, TollgateDrop#tollgate_drop.rand_val),
      ?FILE_LOG_DEBUG("calcutlate drop is = ~p", [CalDropList]),
      %%check_drop_equal(DropList, CalDropList);
      {success, CalDropList};
    true ->  %%无尽模式
      %%根据闯关的个数以及生成的种子，计算宝箱掉落
      %%12-19 修改，根据打死的boss，计算宝箱掉落
      CalDropList = gen_endless_drop(BossCount, Stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.seed_val, Stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.rand_val),
      ?FILE_LOG_DEBUG("endless calculate drop list: ~p", [CalDropList]),
      {success, CalDropList}
  %%check_drop_equal(DropList, CalDropList)
  end.

%%检查掉落：需要检查数量：不低于1个
%% check_drops(Account, DropList, TollgateID, WaveCount) when is_record(Account, account) andalso is_list(DropList) andalso is_integer(TollgateID) andalso is_integer(WaveCount) ->
%%   if
%%     TollgateID < 10000 orelse TollgateID > 20000 ->
%%       ?FILE_LOG_DEBUG("tollgate ~p drop is ~p", [TollgateID, DropList]),
%%       %%根据关卡开始之前的种子算三个掉落
%%       TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
%%       TollgateDropList =
%%         [
%%           {TollgateConfig#res_stage.drop_1, TollgateConfig#res_stage.drop_1_prob},
%%           {TollgateConfig#res_stage.drop_2, TollgateConfig#res_stage.drop_2_prob},
%%           {TollgateConfig#res_stage.drop_3, TollgateConfig#res_stage.drop_3_prob}
%%         ],
%%       TollgateDrop = Account#account.stage#stage.tollgate_drop,
%%       CalDropList = gen_tollgate_drop(TollgateDropList, TollgateDrop#tollgate_drop.seed_val, TollgateDrop#tollgate_drop.rand_val),
%%       ?FILE_LOG_DEBUG("calcutlate drop is = ~p", [CalDropList]),
%%       check_drop_equal(DropList, CalDropList);
%%     true ->  %%无尽模式
%%       %%根据闯关的个数以及生成的种子，计算宝箱掉落
%%       %%12-19 修改，根据打死的boss，计算宝箱掉落
%%       CalDropList = gen_endless_drop(WaveCount, Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.seed_val, Account#account.stage#stage.endless_tollgate#endless_tollgate.endless_drop#endless_drop.rand_val),
%%       ?FILE_LOG_DEBUG("endless calculate drop list: ~p", [CalDropList]),
%%       check_drop_equal(DropList, CalDropList)
%%   end.

gen_tollgate_drop(TollgateDropList, Seed, RandV) when is_list(TollgateDropList) andalso is_integer(Seed) andalso is_integer(RandV) andalso Seed > 0 andalso is_integer(RandV)->
  gen_tollgate_drop_1(TollgateDropList, Seed, RandV, []).

gen_tollgate_drop_1([], _, _, OutList) -> lists:reverse(OutList);
gen_tollgate_drop_1([{DropID, TollgateProb} | T], Seed, RandV, OutList) ->
  {RandValue, NextV} = dd_util:random(Seed, RandV),
  Prob = RandValue rem 10000,
  ?FILE_LOG_DEBUG("gen_tollgate_drop: this rand = ~p, prob=~p, randNext = ~p", [RandV, Prob, NextV]),
  if
    DropID =:= "" orelse Prob > TollgateProb ->  gen_tollgate_drop_1(T, Seed, NextV, OutList);
    true -> gen_tollgate_drop_1(T, Seed, NextV, [{DropID, 1} | OutList])
  end.


gen_endless_drop(WaveCount, Seed, RandV) when is_integer(WaveCount) andalso is_integer(Seed) andalso is_integer(RandV) andalso Seed > 0 andalso is_integer(RandV) ->
  if
    WaveCount < 0 orelse WaveCount > 250 ->
      ?FILE_LOG_DEBUG("endless wave count error ~p", [WaveCount]),
      throw({custom, "HintRequestDataError"});
    true -> ok
  end,
 %% Count = EndlessCount div 3,
  %%12-19 修改最新机制
  %%12-22 修改机制
  dd_util:statistics_list(gen_endless_drop_1(WaveCount, 1, Seed, RandV, []), integer).

%% check_drop_equal(DropList, CalDropList) when is_list(DropList) andalso is_list(CalDropList) ->
%%   Len1 = length(DropList),
%%   Len2 = length(CalDropList),
%%   if
%%     Len1 =/= Len2 ->
%%       ?FILE_LOG_DEBUG("drop count not equal", []),
%%       throw({custom, "HintRequestDataError"});
%%     Len1 =:= 0 -> true;
%%     true ->
%%       Tree = lists:foldl(fun({Id, Cnt}, TmpTree) -> gb_trees:insert(Id, Cnt, TmpTree) end, gb_trees:empty(), CalDropList),
%%       lists:foreach(
%%         fun({ID, Count}) ->
%%           case gb_trees:lookup(ID, Tree) of
%%             none ->  %%不存在
%%               ?FILE_LOG_ERROR("drop id =~p error",[ID]),
%%               throw({custom, "HintRequestDataError"});
%%             {value, Count} -> ok;
%%             _ ->
%%               ?FILE_LOG_ERROR("endless drop id =~p count = ~p count error",[ID, Count]),
%%               throw({custom, "HintRequestDataError"})
%%           end
%%         end, DropList)
%%   end.


gen_endless_drop_1(0, _, _, _, OutList) -> OutList;
gen_endless_drop_1(Count, WaveID, Seed, RandV, OutList) ->
  EndlessWaveConfig = cache_csv:get_endless_config_by_wave_id(WaveID),
  {RandValue, NextV} = dd_util:random(Seed, RandV),
  Prob = RandValue rem 10000,
  ?FILE_LOG_DEBUG("gen_endless_drop: this rand = ~p, prob=~p, randNext = ~p", [RandV, Prob, NextV]),
  if
    Prob > EndlessWaveConfig#res_endless_config.drop_probability  ->  gen_endless_drop_1(Count - 1, WaveID + 1, Seed, NextV, OutList);
    true -> gen_endless_drop_1(Count - 1, WaveID + 1, Seed, NextV, [EndlessWaveConfig#res_endless_config.drop_chest_id | OutList])
  end.


check_star_score(DataStat, Score, Star) when is_record(DataStat, game_end_statistics) andalso is_integer(Score) andalso is_integer(Star) ->
  success.

check_tollgate(TollgateID, Stage) when is_integer(TollgateID) andalso is_record(Stage, stage) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  try
    if
      TollgateConfig#res_stage.pre_tollgate_id =:= 0 -> true;
      true ->
        {success, _Item} = cache_util:find_finished_tollgate_by_id(Stage, TollgateConfig#res_stage.pre_tollgate_id)
    end
  catch
     _:_ ->
       ?FILE_LOG_ERROR("tollgate not unlock [~p]", [TollgateID]),
       throw({custom, "HintTollgateUnlock"})
  end,
  if
    TollgateID < 10000 -> %%闯关模式关卡,有单日限制次数
      if
        TollgateID > ?MAX_TOLLGATE -> throw({custom, "HintTollgateUnlock"});
        true -> ok
      end,
       case cache_util:find_finished_tollgate_by_id(Stage, TollgateID) of
         {success, TollgateItem} ->
           CTime = dd_util:timestamp(),
           LastFinishTs = TollgateItem#tollgate.last_pass_ts,
           TDif = CTime - LastFinishTs,
           if
             TDif > (TollgateConfig#res_stage.cool_time - 10) -> ok;
             true ->
               throw({custom, "HintInvalidRequest"})
           end,
           if
             TollgateItem#tollgate.daily_remain_times > 0 -> ok;
             TollgateItem#tollgate.addition_remain_times > 0 -> ok;
             true ->
               throw({custom, "HintTollgateNoTimes"})
           end;
         fail -> ok
       end;
    TollgateID < 20000 -> %%无尽模式
      ok;
    TollgateID < 30000 -> %%活动关卡
      case cache_util:find_activity_tollgate_by_id(Stage#stage.ac_tollgate, TollgateID) of
        {success, Item} ->
          CurTime = dd_util:timestamp(),
          LastFinishTs = Item#activity_tollgate_item.last_finish_ts,
          TimeDif = CurTime - LastFinishTs,
          if
            TimeDif > (TollgateConfig#res_stage.cool_time - 10) -> ok;
            true ->
              throw({custom, "HintInvalidRequest"})
          end,
          if
            Item#activity_tollgate_item.daily_remain_times > 0 -> ok;
            true ->
              throw({custom, "HintTollgateNoTimes"})
          end;
        fail -> ok
      end;
    true -> ok
  end.

get_tollgate_game_end_type(TollgateID) when is_integer(TollgateID) andalso TollgateID > 0 andalso TollgateID < 30000 ->
  if
    TollgateID < 10000 -> <<"game_end">>;
    TollgateID < 20000 -> <<"endless_game_end">>;
    TollgateID < 30000 -> <<"activity_game_end">>
  end.

check_strength(TollgateId, Stage, UserStrength) when is_integer(TollgateId) andalso is_integer(UserStrength) andalso is_record(Stage, stage) ->
  CostEnergy = get_tollgate_energy_cost(TollgateId, Stage),
  if
    CostEnergy > UserStrength ->
      ?FILE_LOG_ERROR("have not sufficient strength, [~p]", [TollgateId]),
      throw({custom, "HintInsufficientEnergy"});
    true -> ok
  end.

%%扫荡关卡消耗体力，扫荡状态另处理
get_sweep_energy_cost(TollgateID, Stage, SweepTimes) when TollgateID < 10000 andalso is_record(Stage, stage) andalso SweepTimes > 0 ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  DailySweepCostL = TollgateConfig#res_stage.normal_sweep_energy,
  AdditionSweepCostL = TollgateConfig#res_stage.addition_sweep_energy,
  DailyTimes = TollgateConfig#res_stage.daily_restrict_count,
  DailyAddition = TollgateConfig#res_stage.daily_addition_count,
  %%累加（有待改进）
  case cache_util:find_finished_tollgate_by_id(Stage, TollgateID) of
    {success, Tollgate} ->
      DailyRemainTimes = Tollgate#tollgate.daily_remain_times,
      DailyAdditionRemain = Tollgate#tollgate.addition_remain_times,
      if
        DailyRemainTimes > 0 ->
          Start = DailyTimes - DailyRemainTimes + 1,
          End = Start + SweepTimes - 1,
          lists:foldl(
            fun(Index, TmpSum) ->
              TmpSum + proplists:get_value(Index, DailySweepCostL, undefined)
            end, 0, lists:seq(Start, End));
        DailyAdditionRemain > 0 ->
          Start1 = DailyAddition - DailyAdditionRemain + 1,
          End1 = Start1 + SweepTimes - 1,
          lists:foldl(
            fun(Index, TmpSum) ->
              TmpSum + proplists:get_value(Index, AdditionSweepCostL, undefined)
            end, 0, lists:seq(Start1, End1));
        true -> throw({custom, "HintTollgateNoTimes"})
      end;
    fail ->
      Start2 = 1,
      End2 = Start2 + SweepTimes - 1,
      lists:foldl(
        fun(Index, TmpSum) ->
          TmpSum + proplists:get_value(Index, DailySweepCostL, undefined)
        end, 0, lists:seq(Start2, End2))
  end.


%%非扫荡关卡消耗体力，扫荡状态另处理
-spec(get_tollgate_energy_cost(TollgateID::integer(), Param::any()) -> CostEnergy::integer()).
get_tollgate_energy_cost(TollgateId, Stage) when TollgateId < 10000 andalso is_record(Stage, stage) ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateId),
  DailyCostStrengthL = TollgateConfig#res_stage.normal_cost_energy,
  AdditionCostEnergyL = TollgateConfig#res_stage.addition_cost_energy,
  DailyTimes = TollgateConfig#res_stage.daily_restrict_count,
  DailyAddition = TollgateConfig#res_stage.daily_addition_count,
  CostEnergy =
    case cache_util:find_finished_tollgate_by_id(Stage, TollgateId) of
      {success, Tollgate} ->
        DailyRemainTimes = Tollgate#tollgate.daily_remain_times,
        DailyAdditionRemain = Tollgate#tollgate.addition_remain_times,
        if
          DailyRemainTimes > 0 -> proplists:get_value(DailyTimes - DailyRemainTimes + 1, DailyCostStrengthL, undefined);
          DailyAdditionRemain > 0 -> proplists:get_value(DailyAddition - DailyAdditionRemain + 1, AdditionCostEnergyL, undefined);
          true -> throw({custom, "HintTollgateNoTimes"})
        end;
      fail ->
        proplists:get_value(1, DailyCostStrengthL, undefined)
    end,
  case CostEnergy of
    undefined -> throw({custom, "HintSystemDataError"});
    _ -> CostEnergy
  end;
get_tollgate_energy_cost(TollgateID, _) when is_integer(TollgateID) andalso TollgateID < 30000 andalso TollgateID > 20000 ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  TollgateConfig#res_stage.cost_strength;
get_tollgate_energy_cost(TollgateID, _) when is_integer(TollgateID) andalso TollgateID < 20000 andalso TollgateID > 10000 ->
  TollgateConfig = cache_csv:get_tollgate_config(TollgateID),
  TollgateConfig#res_stage.cost_strength.


check_props(PropList, Backpack) when is_list(PropList) andalso is_record(Backpack, backpack) ->
  PropTree = Backpack#backpack.prop_list,
  lists:foreach(
    fun({Id, Count}) ->
      case gb_trees:lookup(Id, PropTree) of
        none ->
          ?FILE_LOG_ERROR("property not exist: [~p]", [Id]),
          throw({custom, "HintRequestDataError"});
        {value, Num} ->
          if
            Count > Num ->
              ?FILE_LOG_ERROR("have not sufficient property; [~p] count = [~p]", [Id, Count]),
              throw({custom, "HintInsufficientTool"});
            true -> ok
          end
      end
    end, PropList).

get_login_account(Uin) ->
  case cache_data_mgr:get_account(Uin) of
    {success, {Account, IsCreate}} -> {success, {Account, IsCreate}};
    Other ->
      ?FILE_LOG_DEBUG("get_login_account: error, reason = ~p",[Other]),
      throw({custom, "HintSystemError"})
  end.

get_account_without_create(Uin) ->
  case cache_data_mgr:get_account_without_create(Uin) of
    {success, Account} -> {success, Account};
    Other  ->
      ?FILE_LOG_DEBUG("get_login_account: error, reason = ~p",[Other]),
      throw({custom, "HintSystemError"})
  end.

get_account(Uin) ->
  case cache_data_mgr:get_account(Uin) of
    {success, {Account, _}} -> {success, Account};
    Other  ->
      ?FILE_LOG_DEBUG("get_login_account: error, reason = ~p",[Other]),
      throw({custom, "HintSystemError"})
  end.

update_account(Account, FieldList) ->
  if
    length(FieldList) =< 0 -> success;
    true ->
      case cache_data_mgr:update_account(Account, FieldList, false) of
        success ->  success;
        Other ->
          ?FILE_LOG_ERROR("update account error: [~p]", Other),
          throw({custom, "HintSystemError"})
      end
  end.

update_account_immediately(Account, FieldList) ->
  if
    length(FieldList) =< 0 -> success;
    true ->
      case cache_data_mgr:update_account(Account, FieldList, true) of
        success ->  success;
        Other ->
          ?FILE_LOG_ERROR("update account error: [~p]", Other),
          throw({custom, "HintSystemError"})
      end
  end.

del_account(Uin) when is_integer(Uin) ->
  {success, _Account} = get_account(Uin),
  case cache_data_mgr:delete_account(Uin) of
    success ->  success;
    Other ->
      ?FILE_LOG_ERROR("delete account error: [~p]", Other),
      throw({custom, "HintSystemError"})
  end.

add_charge_order(Uin, Order) when is_integer(Uin) andalso is_list(Order) ->
  case cache_data_mgr:add_charge_order(Uin, Order) of
    success ->  success;
    repeat ->
      ?FILE_LOG_DEBUG("repeat order :~p", [Order]),
      throw({custom, "RepeatOrder"});
    fail ->
      ?FILE_LOG_DEBUG("add_charge_order error", []),
      throw({custom, "AddChargeError;" ++ dd_util:to_list(Order)})
  end.

get_charge_order(Uin, Order) when is_integer(Uin) andalso is_list(Order) ->
  case cache_data_mgr:get_charge_order(Uin, Order) of
    not_exist -> not_exist;
    {success, Info} -> {success, Info};
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("get_charge_order error, [~p, ~p, ~p]", [Uin, Order, Reason]),
      throw({custom, "HintSystemError"})
  end.


%%{NAccount, FiledlList}
update_account_to_time(Account) when is_record(Account, account) ->
  StrengthItem = cache_update_to_time:update_strength(Account#account.strength),  %%更新体力值
  StageItem = cache_update_to_time:update_harvest_obstacle(Account#account.stage),   %%更新收成障碍物
  Ts_rm_ticket = cache_update_to_time:update_ts_info(Account#account.reward_match_ts,?TS_RM_INIT,?TS_RM_CD,"reward_match_ts"),%%更新赏金赛入场券
  
  %%TEMP
  %%Shop = cache_update_to_time:update_daily_shop(Account#account.shop),

  L = [StageItem, StrengthItem, Ts_rm_ticket],
  {NAccount, NFieldList} =
    lists:foldr(
     fun({Name, Value, IsUpdate}, {TmpAccount, FieldList}) ->
        if
          Name =:= "strength" andalso IsUpdate =:= true -> {TmpAccount#account{strength = Value}, [Name | FieldList]};
          Name =:= "stage" andalso IsUpdate =:= true -> {TmpAccount#account{stage = Value}, [Name | FieldList]};
          %%Name =:= "shop" andalso  IsUpdate =:= true -> {TmpAccount#account{shop = Shop}, [Name | FieldList]};
		  Name =:= "reward_match_ts" andalso IsUpdate =:= true -> {TmpAccount#account{reward_match_ts = Value}, [Name | FieldList]};
          true ->
            ?FILE_LOG_WARNING("update_account_to_time: [~p] no update", [Name]),
            {TmpAccount, FieldList}
        end
     end, {Account, []}, L),
  %%{NNAccount, TFieldList} = cache_mission:sync_config(NAccount),
  %%NNFieldList = lists:umerge3(TFieldList, NFieldList, []),
  ?FILE_LOG_DEBUG("update_account_to_time, ~p", [NFieldList]),
  {NAccount, NFieldList}.

update_equipment(Account) when is_record(Account, account) ->
  HeroList =
    lists:map(
      fun(TowerItem) ->
        Sign =
          lists:foldr(
            fun(EquipItem, Tmp) ->
              case cache_csv:get_equipment_config_without_exception(EquipItem#equipment.no) of
                {success, _} -> Tmp;
                {fail, _} -> false
              end
            end, true, TowerItem#character.equipment_list),
        case Sign of
          true ->  TowerItem;
          false ->
            TowerConfig = cache_csv:get_tower_config(TowerItem#character.id),
            [W1, W2, W3] = TowerItem#character.equipment_list,
            NEquipList = [W1#equipment{no = TowerConfig#res_tower.base_hat_id}, W2#equipment{no = TowerConfig#res_tower.base_weapon_id}, W3#equipment{no = TowerConfig#res_tower.base_second_weapon_id}],
            TowerItem#character{equipment_list = NEquipList}
        end
      end, Account#account.heros#heros.character_lists),

  BackpackEquipList =
    lists:foldr(
      fun(EquipItem, TmpList) ->
        case cache_csv:get_equipment_config_without_exception(EquipItem#equipment.no) of
          {success, _} -> [EquipItem | TmpList];
          {fail, _} -> TmpList
        end
      end, [], Account#account.backpack#backpack.equipment_list),

  NBackpack = Account#account.backpack#backpack{equipment_list = BackpackEquipList},
  Hero = Account#account.heros#heros{character_lists = HeroList},
  {success, Account#account{backpack = NBackpack, heros = Hero}}.

update_equipment_exp(OAccount) when is_record(OAccount, account) ->
  {success, Account} = update_equipment(OAccount),
  MaxLevel = cache_api:get_equipment_max_level(Account),
  TotalExp = cache_api:get_equipment_total_exp(MaxLevel - 1),
  HeroList =
    lists:map(
    fun(TowerItem) ->
        EquipList =
          lists:map(
            fun(EquipItem) ->
              if
                EquipItem#equipment.exp =< 0 ->
                  EquipItem#equipment{exp = 0};
                EquipItem#equipment.exp > TotalExp ->
                  EquipItem#equipment{exp = TotalExp};
                true ->
                  EquipItem
              end
            end, TowerItem#character.equipment_list),
      TowerItem#character{equipment_list = EquipList}
    end, Account#account.heros#heros.character_lists),

  BackpackEquipList =
    lists:map(
      fun(EquipItem) ->
        TotalExp = cache_api:get_equipment_total_exp(MaxLevel - 1),
        if
          EquipItem#equipment.exp =< 0 ->
            EquipItem#equipment{exp = 0};
          EquipItem#equipment.exp > TotalExp ->
            EquipItem#equipment{exp = TotalExp};
          true ->
            EquipItem
        end
      end, Account#account.backpack#backpack.equipment_list),

  NBackpack = Account#account.backpack#backpack{equipment_list = BackpackEquipList},
  Hero = Account#account.heros#heros{character_lists = HeroList},
  {Account#account{backpack = NBackpack, heros = Hero}, ["backpack", "hero"]}.

update_inscription(Account) when is_record(Account, account) ->
  %%更新角色上的铭文数据
  HeroList =
    lists:map(
      fun(TowerItem) ->
        InscriptionList =
          lists:foldl(
            fun(Ins, TmpList) ->
              case cache_csv:get_inscription_by_id_without_exception(dd_util:to_list(Ins#inscription.id)) of
                {success, _} -> [Ins | TmpList];
                fail -> TmpList
              end
            end, [], TowerItem#character.inscription_list),
        TowerItem#character{inscription_list = InscriptionList}
      end, Account#account.heros#heros.character_lists),
  %%背包材料
  BpInscriptionList =
    lists:foldl(
      fun(Ins, TmpList) ->
        case cache_csv:get_inscription_by_id_without_exception(dd_util:to_list(Ins#inscription.id)) of
          {success, _} -> [Ins | TmpList];
          fail -> TmpList
        end
      end, [], Account#account.backpack#backpack.inscription_list),
  BpInscriptionPieceTree =
    lists:foldl(
      fun({ID, Cnt}, TmpTree) ->
        case cache_csv:get_inscription_piece_config_by_id_without_exception(ID) of
          {success, _} ->
            gb_trees:insert(ID, Cnt, TmpTree);
          fail -> TmpTree
        end
      end, gb_trees:empty(), gb_trees:to_list(Account#account.backpack#backpack.inscription_piece_list)),
  NBackpack = Account#account.backpack#backpack{inscription_list = BpInscriptionList, inscription_piece_list = BpInscriptionPieceTree},
  Heros = Account#account.heros#heros{character_lists = HeroList},
  {Account#account{backpack = NBackpack, heros = Heros}, ["backpack", "hero"]}.

%%更新登陆奖励,更新每日刷新的项目 ,同步数据
update_login(Account) when is_record(Account, account) ->
  LoginReward = Account#account.login_reward,
  {NewLoginReward, IsTodayFirstLogin} = cache_update_to_time:update_login(LoginReward),
  NAccount = update_platform_info(Account),
  %%更新好友数量的成就
  FriendNum = length(NAccount#account.platform_info#platform_info.player_friends),
  {_UpdateAchievementList, Achievement} =
      if
        FriendNum > 0 -> cache_mission:update_achievement(friend_number, NAccount#account.uin, NAccount#account.achievement, {Account#account.uin, FriendNum});
        true -> {[], NAccount#account.achievement}
      end,
  NNAccount = NAccount#account{achievement = Achievement},

  case IsTodayFirstLogin of
    false ->
      {NNAccount#account{login_reward = NewLoginReward, last_login_ts = dd_util:timestamp()}, ["login_reward", "last_login_ts", "platform_info", "achievement"]};
    true ->    %%更新每日刷新项目
      {NNNAccount, FieldList} = update_daily(NNAccount),
      %%发放奖励
      deliver_platform_continuous_login_reward(Account#account.uin, false),
      {NewAccount, NFieldList} = cache_mission:sync_config(NNNAccount),
      {NewAccount#account{login_reward = NewLoginReward, last_login_ts = dd_util:timestamp()}, lists:umerge3(FieldList, ["login_reward", "last_login_ts", "platform_info", "achievement"], NFieldList)}
   end.

update_daily(Account) when is_record(Account, account) ->
  ?FILE_LOG_DEBUG("update daily login item ~p, ~p",[Account#account.uin, dd_util:time_format()]),
  Shop = cache_update_to_time:update_daily_shop(Account#account.shop),
  Mission = cache_update_to_time:update_daily_mission(Account#account.mission),
  Achievement = cache_update_to_time:update_daily_achievement(Account#account.achievement),     %%仅限于测试阶段使用，上线后注释掉，同步成就配置文件
  NAchievement = cache_mission:update_achievement_daily(Achievement),                            %%仅限于测试阶段使用，上线后注释掉，同步成就进度值
  Stage = cache_update_to_time:update_daily_tollgate(Account#account.stage),
  Strength = cache_update_to_time:update_daily_strength(Account#account.strength),
  Lottery = cache_update_to_time:update_daily_lottery(Account#account.lottery),
  {Account#account{mission = Mission, shop = Shop, achievement = NAchievement, stage = Stage, strength = Strength, lottery = Lottery}, ["mission", "shop", "achievement", "stage", "strength", "lottery"]}.

update_platform_info(Account) when is_record(Account, account)->
  case dispatch_server_req:sync_register_user_data(Account#account.uin) of
    {success,  {UName, DisName, _Device, Plat, FriendList}} ->
      PlatForm = #platform_info{plat_type = Plat, player_friends = FriendList, player_id = UName, player_dis_name = DisName},
      if
        DisName =:= Account#account.platform_info#platform_info.player_dis_name -> ok;
        true ->
          ?FILE_LOG_DEBUG("player info change, notify rank", []),
          platform_info_modify_notify(Account#account.uin, PlatForm)
      end,
      Account#account{platform_info = PlatForm};
    {fail, Reason} ->
      ?FILE_LOG_ERROR("sync_register_user_data ERROR, reason = [~p]", [Reason]),
      Account
  end.

%%通知相关节点和数据（排名）
platform_info_modify_notify(Uin, PlatForm) when is_record(PlatForm, platform_info) andalso is_integer(Uin) ->
  try
    {success, RankNode} = dd_ms:read_config(ranking_node),
    rpc:cast(RankNode, ranking_tollgate, update_player_basic_info, [Uin, {PlatForm#platform_info.player_id, PlatForm#platform_info.player_dis_name}])
  catch
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()])
  end.

get_lottery_config() ->
  {success,Single} = cache_configure_data:get_lottery_config_data(1),
  {success,Ten} = cache_configure_data:get_lottery_config_data(2),
  [Single, Ten].
get_lottery_config(SingleLotteryTimes) ->
  [Single, Ten] = get_lottery_config(),
  NSingle =
    if
      SingleLotteryTimes =< 0 -> Single#lottery_conf{price = 0};
      true -> Single
    end,
  [NSingle, Ten].
get_lottery_config(Type, SingleLotteryTimes) ->
    if
      Type =:= 1 andalso SingleLotteryTimes =< 0 -> %%首抽
        {success, Lottery} = cache_configure_data:get_lottery_config_data(1),
         Lottery#lottery_conf{price = 0, id = 0};
      true ->
        {success, Lottery} = cache_configure_data:get_lottery_config_data(Type),
        Lottery
    end.

%% get_shop_discount()->  [].
%% %%  ShopList = cache_configure_data:get_commodity_from_ets(),
%% %%  ShopList.

get_cache_node(Uin) when is_integer(Uin)->
  {success, CacheHashRule} = dd_ms:read_config(cache_hash_rule),
  case hash_service_util:find_key_store_node(dd_util:to_list(Uin), CacheHashRule) of
    fail ->
      ?FILE_LOG_ERROR("no available data node, ~p", [Uin]),
      throw({custom, "HintSystemError"});
    {success, Node} when is_atom(Node) -> {success, Node}
  end.

get_max_tollgate(BaseTollgateList) when is_list(BaseTollgateList) ->
  get_max_tollgate_num(BaseTollgateList, 1).

get_max_tollgate_num([], Num) -> Num;
get_max_tollgate_num([H | T], Num) ->
  if
    Num < H#tollgate.id -> get_max_tollgate_num(T, H#tollgate.id);
    true -> get_max_tollgate_num(T, Num)
  end.


%%发放连续登陆的奖励，（针对不同的平台玩家，发放的奖励不同）
deliver_platform_continuous_login_reward(Uin, true) ->
  try
    ConfigList = cache_csv:get_all_continuous_login_reward_config(),
    lists:foreach(
      fun(RewardConfig) ->
        case RewardConfig#res_continuous_login_reward.reward_player_type of
          1 -> %%新玩家奖励
            deliver_reward(RewardConfig, Uin);
          2 ->  %%登陆奖励但不奖励新玩家
            ok;
          _ ->
            deliver_reward(RewardConfig, Uin)
        end
      end, ConfigList)
  catch
    What:Type ->
      ?FILE_LOG_ERROR("deliver platform continuous login reward error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()])
  end;
deliver_platform_continuous_login_reward(Uin, false) ->
  try
    ConfigList = cache_csv:get_all_continuous_login_reward_config(),
    lists:foreach(
      fun(RewardConfig) ->
        case RewardConfig#res_continuous_login_reward.reward_player_type of
          1 -> %%新玩家奖励
            ok;
          2 ->  %%登陆奖励但不奖励新玩家
            deliver_reward(RewardConfig, Uin);
          _ ->
            deliver_reward(RewardConfig, Uin)
        end
      end, ConfigList)
  catch
    What:Type ->
      ?FILE_LOG_ERROR("deliver platform continuous login reward error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()])
  end.

deliver_reward(RewardConfig, Uin) when is_record(RewardConfig, res_continuous_login_reward) andalso is_integer(Uin) ->
  {success, MailNode} = dd_ms:read_config(mail_node),
  CurTime = dd_util:timestamp(),
  if
    CurTime > RewardConfig#res_continuous_login_reward.reward_start_ts
      andalso CurTime =< RewardConfig#res_continuous_login_reward.reward_over_ts ->
      case rpc:call(MailNode, mail, add_attach_mail,
        [-1, Uin, "default", [RewardConfig#res_continuous_login_reward.reward_desc],
          {RewardConfig#res_continuous_login_reward.reward_type, RewardConfig#res_continuous_login_reward.reward_prop_id, RewardConfig#res_continuous_login_reward.reward_count}, 7]) of
        {success, _} -> success;
        Other ->
          ?FILE_LOG_ERROR("~p continuous login reward, deliver reward error, reward id = ~p, reason = ~p", [Uin, RewardConfig#res_continuous_login_reward.id, Other])
      end;
    true ->
      ?FILE_LOG_DEBUG("~p continuous login reward over date, reward id = ~p, reward start time = ~p, reward over time",
        [Uin, RewardConfig#res_continuous_login_reward.id, dd_util:to_local_time(dd_util:timestamp_to_datetime(RewardConfig#res_continuous_login_reward.reward_start_ts)),
          dd_util:to_local_time(dd_util:timestamp_to_datetime(RewardConfig#res_continuous_login_reward.reward_over_ts))])
  end.

