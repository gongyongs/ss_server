%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 八月 2014 下午2:32
%%%-------------------------------------------------------------------
-module(cache_util).
-author("zqlt").

-include("cache_def.hrl").

%% API
-export([
  find_player_by_id/2,
  find_login_reward/2,
  find_world_map_block/2,
  find_tower_by_id/2,
  find_equipment_by_id/2,
  find_finished_tollgate_by_id/2,
  find_achievement_by_id/2,
  find_achievement_group_by_id/2,
  find_commodity_by_id/2,
  find_mission_by_id/2,
  find_activity_tollgate_by_id/2,
  find_friend_strength_gift_by_id/2,
  find_friend_by_uin/2,
  find_friend_by_id/2,
  find_guide_step_by_id/2,
  find_cdkey_pack_by_id/2,
  find_inscription_by_id/2,
  find_inscription_by_pos/2
]).

-export([
  update_tower/2,
  update_equipment/2,
  update_tollgate/2,
  update_harvest_obstacle/2,
  update_mission/2,
  update_achievement/2,
  update_achievement_group/2,
  update_commodity/2,
  update_activity_tollgate/2,
  update_guide_step/2,
  update_cdkey_pack/2,
  update_inscription_by_pos/2
]).

-export([
  delete_login_reward_by_id/2,
  delete_equipment_by_id/2,
  delete_mission_by_id/2,
  delete_inscription_by_id/2
]).

-export([
  find_item_by_id/3,
  update_list_item/3,
  delete_list_item_by_id/3
]).

-export([
  insert_achievement_by_id/3,
  insert_achievement_group_by_id/3,
  insert_mission_by_id/3
]).

insert_achievement_by_id(AchievementList, AchievementItem, IsUpdate) when is_list(AchievementList) andalso is_record(AchievementItem, achievement_item)  ->
  case find_achievement_by_id(AchievementList, AchievementItem#achievement_item.id) of
    {success, _} ->
      case IsUpdate of
        false -> AchievementList;
        true -> update_achievement(AchievementList, AchievementItem)
      end;
    fail -> [AchievementItem | AchievementList]
  end.

insert_achievement_group_by_id(GroupList, Group, IsUpdate) when is_list(GroupList) andalso is_record(Group, achievement_group) ->
  case find_achievement_group_by_id(GroupList, Group#achievement_group.group_id) of
    {success, _} ->
      case IsUpdate of
        false -> GroupList;
        true -> update_achievement_group(GroupList, Group)
      end;
    fail -> [Group | GroupList]
  end.

insert_mission_by_id(List, Item, IsUpdate) when is_list(List) andalso is_record(Item, mission_item) ->
  case find_mission_by_id(List, Item) of
    {success, _} ->
      case IsUpdate of
        false -> List;
        true -> update_mission(List, Item)
      end;
    fail -> [Item | List]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_player_by_id(PlayerList, PlayerId) when is_list(PlayerList) andalso is_integer(PlayerId) ->
  F = fun(Player, ID) -> Player#character.id =:= ID end,
  find_item_by_id(PlayerList, F, PlayerId).

find_inscription_by_id(List, ID) when is_list(List) andalso is_list(ID) ->
  F = fun(Item, Key) -> Item#inscription.id =:= Key end,
  find_item_by_id(List, F, ID).

find_inscription_by_pos(List, Pos) when is_list(List) andalso is_integer(Pos) ->
  F = fun(Item, Key) -> Item#inscription.pos =:= Key end,
  find_item_by_id(List, F, Pos).

find_cdkey_pack_by_id(List, ID) when is_list(List) andalso is_list(ID) ->
  F = fun(Item, Key) -> Item#record_item.key =:= Key end,
  find_item_by_id(List, F, ID).

find_guide_step_by_id(List, ID) when is_list(List) andalso is_list(ID) ->
  F = fun(Item, Key) -> Item#record_item.key =:= Key  end,
  find_item_by_id(List, F, ID).

find_friend_by_uin(List, Uin) when is_list(List) andalso is_integer(Uin) ->
  F = fun(FriendItem, ID) -> FriendItem#friend_item.uin =:= ID end,
  find_item_by_id(List, F, Uin).

find_friend_by_id(List, FriendId) when is_list(List) andalso is_list(FriendId) ->
  F = fun(FriendItem, ID) -> FriendItem#friend_item.id =:= ID  end,
  find_item_by_id(List, F, FriendId).

find_friend_strength_gift_by_id(List, Id) when is_list(List) andalso is_integer(Id) ->
  F = fun(StrengthGiftItem, ID) -> StrengthGiftItem#strength_item.give_strength_dest =:= ID end,
  find_item_by_id(List, F, Id).

find_commodity_by_id(List, ID) when is_list(List) andalso is_list(ID) ->
  F = fun(CommodityItem, ItemID) -> CommodityItem#goods.id =:= ItemID  end,
  find_item_by_id(List, F, ID).

find_mission_by_id(List, Item) when is_list(List) andalso is_record(Item, mission_item) ->
  F = fun(MissionItem, ID) -> MissionItem#mission_item.mission_id =:= ID end,
  find_item_by_id(List, F, Item#mission_item.mission_id);
find_mission_by_id(List, MissionID) when is_list(List) andalso is_list(MissionID) ->
  F = fun(MissionItem, ID) -> MissionItem#mission_item.mission_id =:= ID end,
  find_item_by_id(List, F, MissionID).

find_achievement_by_id(AchievementList, AchievementID) when is_list(AchievementList) andalso is_list(AchievementID) ->
  F = fun(Item, ID) ->  Item#achievement_item.id =:= ID end,
  find_item_by_id(AchievementList, F, AchievementID).

find_achievement_group_by_id(GroupList, GroupID) when is_list(GroupList) andalso is_integer(GroupID) ->
  F = fun(GroupItem, ID) ->  GroupItem#achievement_group.group_id =:= ID end,
  find_item_by_id(GroupList, F, GroupID).

find_login_reward(RewardList, Id) when is_list(RewardList) andalso is_list(RewardList) ->
  F = fun(RewardItem, RewardID) -> RewardItem#login_reward_item.id =:= RewardID end,
  find_item_by_id(RewardList, F, Id).

find_world_map_block(BlockList, WBId) when is_list(BlockList) andalso is_list(WBId) ->
  F = fun(BlockItem, Id) -> BlockItem#harvest_obstacles_item.id =:= Id end,
  find_item_by_id(BlockList, F, WBId).

find_tower_by_id(TowerList, TowerID) when is_list(TowerList) andalso is_list(TowerID) ->
  F = fun(TowerItem, ID) -> TowerItem#character.id =:= ID end,
  find_item_by_id(TowerList, F, TowerID).

find_equipment_by_id(EquipmentList, EquipmentID) when is_list(EquipmentList)  andalso is_list(EquipmentID) ->
  F = fun(EquipmentItem, ID) -> EquipmentItem#equipment.id =:= ID end,
  find_item_by_id(EquipmentList, F, EquipmentID).

find_finished_tollgate_by_id(Stage, TollgateID) when is_record(Stage, stage) andalso is_integer(TollgateID) ->
  F = fun(Tollgate, ID) -> Tollgate#tollgate.id =:= ID end,
  find_item_by_id(Stage#stage.base_tollgate_list,F, TollgateID).

find_activity_tollgate_by_id(List, TollgateID) when is_list(List) andalso is_integer(TollgateID) ->
  F = fun(Item, ID) -> Item#activity_tollgate_item.tollgate_id =:= ID  end,
  find_item_by_id(List, F, TollgateID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_inscription_by_pos(List, Inscription) when is_list(List) andalso is_record(Inscription, inscription) ->
  F = fun(Item, NItem) -> Item#inscription.pos =:= NItem#inscription.pos end,
  update_list_item(List, F, Inscription).

update_cdkey_pack(List, Pack) when is_list(List) andalso is_record(Pack, record_item) ->
  F = fun(Item, NItem) -> Item#record_item.key =:= NItem#record_item.key end,
  update_list_item(List, F, Pack).

update_guide_step(List, Step) when is_list(List) andalso is_record(Step, record_item) ->
  F = fun(Item, NItem) -> Item#record_item.key =:= NItem#record_item.key end,
  update_list_item(List, F, Step).

update_activity_tollgate(List, ActivityItem) when is_list(List) andalso is_record(ActivityItem, activity_tollgate_item)->
  F = fun(Item, NItem) -> Item#activity_tollgate_item.tollgate_id =:= NItem#activity_tollgate_item.tollgate_id end,
  update_list_item(List, F, ActivityItem).

update_commodity(GoodsList, GoodsItem) when is_list(GoodsList) andalso is_record(GoodsItem, goods) ->
  F = fun(Item, NItem) -> Item#goods.id =:= NItem#goods.id  end,
  update_list_item(GoodsList, F, GoodsItem).

update_mission(MissionList, MissionItem) when is_list(MissionList) andalso is_record(MissionItem, mission_item) ->
  F = fun(Item, NItem) -> Item#mission_item.mission_id =:= NItem#mission_item.mission_id end,
  update_list_item(MissionList, F, MissionItem).

update_achievement(AchievementList, AchievementItem) when is_list(AchievementList) andalso is_record(AchievementItem, achievement_item) ->
  F = fun(Achievement, NItem) -> Achievement#achievement_item.id =:= NItem#achievement_item.id end,
  update_list_item(AchievementList, F, AchievementItem).

update_achievement_group(AchievementGroupList, AchievementGroup) when is_list(AchievementGroupList) andalso is_record(AchievementGroup, achievement_group) ->
  F = fun(Group, NItem) -> Group#achievement_group.group_id =:= NItem#achievement_group.group_id end,
  update_list_item(AchievementGroupList, F, AchievementGroup).

update_tower(TowerList, TowerItem) when is_list(TowerList) andalso is_record(TowerItem, character) ->
  F = fun(Tower, NItem) -> Tower#character.id =:= NItem#character.id end,
  update_list_item(TowerList, F, TowerItem).

update_equipment(EquipmentList, EquipItem) when is_list(EquipmentList) andalso is_record(EquipItem, equipment)->
  F = fun(Equipment, NItem) -> Equipment#equipment.id =:= NItem#equipment.id end,
  update_list_item(EquipmentList, F, EquipItem).

update_tollgate(Stage, TollgateItem) when is_record(Stage, stage) andalso is_record(TollgateItem, tollgate) ->
  F = fun(Tollgate, NItem) -> Tollgate#tollgate.id =:= NItem#tollgate.id end,
  Stage#stage{base_tollgate_list = update_list_item(Stage#stage.base_tollgate_list, F, TollgateItem)}.

update_harvest_obstacle(BlockList, BlockItem) when is_list(BlockList) andalso is_record(BlockItem, harvest_obstacles_item) ->
  F = fun(Block, NItem) -> Block#harvest_obstacles_item.id =:= NItem#harvest_obstacles_item.id end,
  update_list_item(BlockList, F, BlockItem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_mission_by_id(MissionList, MissionID) when is_list(MissionList) andalso is_list(MissionID) ->
  F = fun(MissionItem, ID) ->  MissionItem#mission_item.mission_id =:= ID  end,
  delete_list_item_by_id(MissionList, F,MissionID).

delete_login_reward_by_id(RewardList, RewardId) when is_list(RewardList) andalso  is_integer(RewardId) ->
  F =  fun(RewardItem, ID) -> RewardItem#login_reward_item.id =:= ID end,
  delete_login_reward_by_id_1(RewardList, F, RewardId).

delete_equipment_by_id(EquipmentList, EquipID) when is_list(EquipmentList) andalso is_list(EquipID) ->
  F = fun(Equipment, ID) -> Equipment#equipment.id =:= ID end,
  delete_list_item_by_id(EquipmentList, F, EquipID).

delete_inscription_by_id(List, InscriptionID) when is_list(InscriptionID) andalso is_list(List) ->
  F = fun(Item, ID) -> Item#inscription.id =:= ID end,
  delete_list_item_by_id(List, F, InscriptionID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_login_reward_by_id_1(List, FunName, Value) when is_list(List) andalso is_function(FunName) ->
  delete_login_reward_by_id_1_1(List, FunName, Value, false, []).
delete_login_reward_by_id_1_1([], _, _, _,Out) -> lists:reverse(Out);
delete_login_reward_by_id_1_1([H | T], FunName, Value, Sign, Out) ->
  case Sign of
    true -> delete_login_reward_by_id_1_1(T, FunName, Value, Sign, [H | Out]);
    false ->
      case FunName(H, Value) of
        true -> delete_login_reward_by_id_1_1(T, FunName, Value, true, Out);
        false -> delete_login_reward_by_id_1_1(T, FunName, Value, false, [H | Out])
      end
  end.

%%查找item根据id
find_item_by_id(List, FunName, Value) when is_list(List) andalso is_function(FunName) ->
  find_item_by_id_1(List, FunName, Value).
find_item_by_id_1([], _, _) -> fail;
find_item_by_id_1([H | T], FunName, Value) ->
  case FunName(H, Value) of
    true -> {success, H};
    false -> find_item_by_id_1(T, FunName, Value)
  end.

update_list_item(List, FunName, Value) when is_list(List) andalso is_function(FunName) ->
  update_list_item_1(List, FunName, Value, []).
update_list_item_1([], _, _, Out) -> lists:reverse(Out);
update_list_item_1([H | T], FunName, Value, Out) ->
  case FunName(H, Value) of
    true -> update_list_item_1(T, FunName, Value, [Value | Out]);
    false -> update_list_item_1(T, FunName, Value, [H | Out])
  end.

%%根据id删除数据
delete_list_item_by_id(List, FunName, Value) when is_list(List) andalso is_function(FunName) ->
  delete_list_item_by_id_1(List, FunName, Value, []).
delete_list_item_by_id_1([], _, _, Out) -> lists:reverse(Out);
delete_list_item_by_id_1([H | T], FunName, Value, Out) ->
  case FunName(H, Value) of
    true -> delete_list_item_by_id_1(T, FunName, Value, Out);
    false -> delete_list_item_by_id_1(T, FunName, Value, [H | Out])
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%