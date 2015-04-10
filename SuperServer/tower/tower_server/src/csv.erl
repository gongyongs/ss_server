%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 七月 2014 上午10:32
%%%-------------------------------------------------------------------
-module(csv).
-author("zqlt").
-include("../deps/file_log/include/file_log.hrl").
-include("csv.hrl").

%% API
-export([
  load_csv_to_cache/1,
  reload_csv_to_cache/1,
  csv_file_list/0
]).
-export([
  to_int/2,
  to_float/2,
  read_field/3,
  load_csv/2,
  load_csv_test/2
]).



to_int(V, DefaultV) when is_list(V) andalso is_integer(DefaultV) ->
  case V of
    [] ->
      DefaultV;
    _  ->
      NewV = check_dot('int', V),
      list_to_integer(NewV)
  end;
to_int(V, _DefaultV) -> V.

to_list(V) ->
  if
    length(V) < 1 -> [];
    true -> V
  end.

to_list_restrict_length(V) ->
  if
    length(V) =< 1  -> [];
    true -> V
  end.

to_float(V, DefaultV) when is_list(V) andalso is_float(DefaultV) ->
  case V of
    [] ->
      DefaultV;
    _  ->
      NewV = check_dot('float', V),
      list_to_float(NewV)
  end;
to_float(V, _DefaultV) -> V.

time_to_ts([], _) -> 0;
time_to_ts(Times, {H, M, S}) ->
  [Y,Month,D] = string:tokens(Times, "/"),
  DateTime = {{dd_util:to_integer(Y), dd_util:to_integer(Month), dd_util:to_integer(D)}, {dd_util:to_integer(H), dd_util:to_integer(M), dd_util:to_integer(S)}},
  dd_util:datetime_to_timestamp(dd_util:to_universal_time(DateTime)).



check_dot('int', V) when is_list(V)->
    case lists:member($., V) of
      true ->
        {A, _B} = lists:splitwith(fun(E) -> E =/= $. end, V),
         A;
      false ->
        V
    end;
check_dot('float', V) when is_list(V) ->
  case lists:member($., V) of
    false ->
      V ++ ".0";
    true ->
      V
  end.

reload_csv_to_cache(Path) ->
  clear(),
  lists:foreach(
    fun(FileName) ->
      load_csv(FileName, Path)
    end,csv_file_list()
  ).

load_csv_to_cache(Path) ->
  init(),
  lists:foreach(
    fun(FileName) ->
      load_csv(FileName, Path)
    end,csv_file_list()
  ).

init() ->
  ets:new(res_equip_config, [set, protected, named_table, {keypos, #res_equipment.id}]),
  ets:new(res_drop_index_table, [set, protected, named_table, {keypos, #res_drop_config.id}]),
  ets:new(res_tower_config, [set, protected, named_table, {keypos, #res_tower.id}]),
  ets:new(res_material_config, [set, protected, named_table, {keypos, #res_material.id}]),
  ets:new(res_init_account, [set, protected, named_table, {keypos, #res_init_account.id}]),
  ets:new(res_stage, [set, protected, named_table, {keypos, #res_stage.id}]),
  ets:new(res_world_map_block, [set, protected, named_table, {keypos, #res_world_map_block.id}]),
  ets:new(res_exp_config, [set, protected, named_table, {keypos, #res_exp_config.id}]),
  ets:new(res_login_reward_config, [set, protected, named_table, {keypos, #res_login_reward.id}]),
  ets:new(res_task_config, [set, protected, named_table, {keypos, #res_task.id}]),
  ets:new(res_achievement_config, [set, protected, named_table, {keypos, #res_achievement.id}]),
  ets:new(res_shop_config, [set, protected, named_table, {keypos, #res_goods.id}]),
  ets:new(res_activity_reward_config, [set, protected, named_table, {keypos, #res_activity_reward.id}]),
  ets:new(res_property_config, [set, protected, named_table, {keypos, #res_property.id}]),
  ets:new(res_template_mail_config, [set, protected, named_table, {keypos, #res_template_mail_config.id}]),
  ets:new(res_language_config, [set, protected, named_table, {keypos, #res_language_config.id}]),
  ets:new(res_treasure_config, [set, protected, named_table, {keypos, #res_treasure_config.group_id}]),
  ets:new(res_endless_config, [set, protected, named_table, {keypos, #res_endless_config.wave_id}]),
  ets:new(res_lottery_config, [set, protected, named_table, {keypos, #res_lottery_config.lottery_type}]),
  ets:new(res_lottery_shop_config, [set, protected, named_table, {keypos, #res_lottery_shop_config.lottery_id}]),
  ets:new(res_shop_package, [set, protected, named_table, {keypos, #res_shop_package.package_id}]),
  ets:new(res_continuous_login_reward, [set, protected, named_table, {keypos, #res_continuous_login_reward.id}]),
  ets:new(res_equip_upgrade_cost, [set, protected, named_table, {keypos, #res_equip_upgrade_cost.id}]),
  ets:new(res_cdkey_package_config,[set, protected, named_table,{keypos, #res_cdkey_package_config.id}]),
  ets:new(res_equip_piece, [set, protected, named_table, {keypos, #res_equip_piece.id}]),
  ets:new(res_inscription_piece, [set, protected, named_table, {keypos, #res_inscription_piece.id}]),
  ets:new(res_inscription, [set, protected, named_table, {keypos, #res_inscription.id}]),
  ets:new(res_tollgate_drop, [set, protected, named_table, {keypos, #res_tollgate_drop.id}]),
  ets:new(res_tollgate_energy, [set, protected, named_table, {keypos, #res_tollgate_energy.tollgate_type}]).

clear() ->
  delete_ets(res_equip_config),
  delete_ets(res_drop_index_table),
  delete_ets(res_tower_config),
  delete_ets(res_material_config),
  delete_ets(res_init_account),
  delete_ets(res_stage),
  delete_ets(res_world_map_block),
  delete_ets(res_exp_config),
  delete_ets(res_login_reward_config),
  delete_ets(res_task_config),
  delete_ets(res_achievement_config),
  delete_ets(res_shop_config),
  delete_ets(res_activity_reward_config),
  delete_ets(res_property_config),
  delete_ets(res_template_mail_config),
  delete_ets(res_language_config),
  delete_ets(res_treasure_config),
  delete_ets(res_endless_config),
  delete_ets(res_lottery_config),
  delete_ets(res_lottery_shop_config),
  delete_ets(res_shop_package),
  delete_ets(res_continuous_login_reward),
  delete_ets(res_equip_upgrade_cost),
  delete_ets(res_cdkey_package_config),
  delete_ets(res_equip_piece),
  delete_ets(res_inscription_piece),
  delete_ets(res_inscription),
  delete_ets(res_tollgate_drop),
  delete_ets(res_tollgate_energy).

delete_ets(TableID) ->
  EtsList = ets:all(),
  case lists:member(TableID, EtsList) of
    true -> ets:delete_all_objects(TableID);
    false -> ok
  end.

csv_file_list() ->
  L = [
    "Tollgate.csv",
    "Equip.csv" ,
    "Tower.csv",
    "Material.csv",
    "InitialAccount.csv",
    "WorldMapBlock.csv",
    "ExpTable.csv",
    "DailyLoginReward.csv",
    "Task.csv",
    "Shop.csv",
    "Achievement.csv",
    "ActivePointReward.csv",
    "Tools.csv",
    "Mail.csv",
    "Treasure.csv",
    "Lottery.csv",
    "EndlessConfig.csv",
    "TreasureConfig.csv",
    "Language.csv",
    "ShopPackage.csv",
    "ContinuousLoginReward.csv",
    "EquipUpgradeCost.csv",
    "CDKeyPackageConfig.csv",
    "EquipPiece.csv",
    "InscriptionPiece.csv",
    "Inscription.csv",
    "TollgateDrop.csv"
  ],
  lists:reverse(L).

read_csv_line(File, LineList) ->
  case file:read_line(File) of
    {ok, Line} ->
      read_csv_line(File, [Line | LineList]);
    eof ->
      NewList = lists:reverse(LineList),
      filter_line_list_title(NewList)
  end.

filter_line_list_title(LineList) ->
  [_Title1, _Title2 | NewTitle] = LineList,
  NewTitle.

read_quotes([$", $\n | _], CharList, FieldList) -> lists:reverse([lists:reverse(CharList) | FieldList]);
read_quotes([$", $, | F], CharList, FieldList) -> read_field(F, [], [lists:reverse(CharList) | FieldList]);
read_quotes([C | F], CharList, FieldList) -> read_quotes(F, [C | CharList], FieldList).

read_field([], CharList, FieldList) -> lists:reverse([lists:reverse(CharList) | FieldList]);
read_field([$" | F], _, FieldList) -> read_quotes(F, [], FieldList);
read_field([$, | F], CharList, FieldList) -> read_field(F, [], [lists:reverse(CharList) | FieldList]);
read_field([$\n | _], CharList, FieldList) -> lists:reverse([lists:reverse(CharList) | FieldList]);
read_field([C | F], CharList, FieldList) -> read_field(F, [C | CharList], FieldList).

load_csv_test("Equip.csv",Path)->
	Time1 =  dd_util:milliseconds(),
  FilePath = Path ++ "Equip.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = re:split(Line,"[,\n]",[{return,list},trim]),
      Value = #res_equipment{
        id = to_list(lists:nth(1, FieldList)),
        name = to_list(lists:nth(2, FieldList)),
        dis_name = to_list(lists:nth(3, FieldList)),
        show_pic = to_list(lists:nth(4, FieldList)),
        bullet_effect_path = to_list(lists:nth(5, FieldList)),
        bullet_effect_name = to_list(lists:nth(6, FieldList)),
        weapon_effect_path = to_list(lists:nth(7, FieldList)),
        weapon_effect_name = to_list(lists:nth(8, FieldList)),
        equip_effect_path = to_list(lists:nth(9, FieldList)),
        equip_effect = to_list(lists:nth(10, FieldList)),
        bullet_speed = to_int(lists:nth(11, FieldList), 0),
        can_attack = to_int(lists:nth(12, FieldList), 0),
        sound_event = to_list(lists:nth(13, FieldList)),
        type = to_int(lists:nth(14, FieldList), 0),
        max_level = to_int(lists:nth(15, FieldList), 0),
        star_level = to_int(lists:nth(16, FieldList), 1),
        belong_tower_type = to_int(lists:nth(17, FieldList), 0),
        upgrade_obj_id1 =  to_list(lists:nth(18, FieldList)),
        upgrade_obj_id2 = to_list(lists:nth(18, FieldList)),
        init_hp = to_int(lists:nth(20, FieldList), 1),
        hp_per_level = to_int(lists:nth(21, FieldList), 1),
        physical_attack_min = to_float(lists:nth(22, FieldList) , 0.0),
        physical_attack_min_per_level = to_float(lists:nth(23, FieldList), 0.0),
        physical_attack_max = to_float(lists:nth(24, FieldList), 0.0),
        physical_attack_max_per_level = to_float(lists:nth(25, FieldList), 0.0),
        magic_attack_min = to_float(lists:nth(26, FieldList), 0.0),
        magic_attack_min_per_level = to_float(lists:nth(27, FieldList), 0.0),
        magic_attack_max = to_float(lists:nth(28, FieldList), 0.0),
        magic_attack_max_per_level = to_float(lists:nth(29, FieldList),0.0),
        crit_attack_level = to_int(lists:nth(30, FieldList), 0),
        crit_attack_grow = to_float(lists:nth(31, FieldList), 0.0),
        hit_level = to_int(lists:nth(32, FieldList), 0),
        hit_grow = to_float(lists:nth(33, FieldList), 0.0),
        speed_level = to_int(lists:nth(34, FieldList), 0),
        speed_grow = to_float(lists:nth(35, FieldList), 0.0),
        skill_id = to_list(lists:nth(36, FieldList)),
        price = to_int(lists:nth(37, FieldList), 0),
        gain_exp = to_int(lists:nth(38, FieldList), 0),
        gain_exp_per_level = to_int(lists:nth(39, FieldList), 0),
        base_dsm = to_int(lists:nth(40, FieldList), 0),
        dsm_per_level = to_int(lists:nth(41, FieldList), 0),
        max_level_1 = to_int(lists:nth(42, FieldList), 0),
        base_score_addition = to_int(lists:nth(43, FieldList), 0),
        base_score_addition_growth = to_float(lists:nth(44, FieldList), 0.0),
        icon = to_list(lists:nth(45, FieldList)),
        advance_cost = to_int(lists:nth(46, FieldList) ,0),
        advance_material_1_id = to_list_restrict_length(lists:nth(47, FieldList)),
        advance_material_1_num = to_int(lists:nth(48, FieldList) ,0),
        advance_material_2_id = to_list_restrict_length(lists:nth(49, FieldList)),
        advance_material_2_num = to_int(lists:nth(50, FieldList), 0),
        advance_material_3_id = to_list_restrict_length(lists:nth(51, FieldList)),
        advance_material_3_num = to_int(lists:nth(52, FieldList), 0),
        advance_material_4_id = to_list_restrict_length(lists:nth(53, FieldList)),
        advance_material_4_num = to_int(lists:nth(54, FieldList), 0),
        advance_material_5_id = to_list_restrict_length(lists:nth(55, FieldList)),
        advance_material_5_num = to_int(lists:nth(56, FieldList), 0)},
        Value
%%         ets:insert(res_equip_config,  Value),
%%         ets:insert(res_drop_index_table, #res_drop_config{id = Value#res_equipment.id, type = 1})
    end, LineList),
	Time2 =  dd_util:milliseconds(),
	io:format("now  in  load csv_test cost ms time is --->~p~n",[Time2-Time1]).



load_csv("Equip.csv", Path) ->
	Time1 =  dd_util:milliseconds(),
  FilePath = Path ++ "Equip.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_equipment{
        id = to_list(lists:nth(1, FieldList)),
        name = to_list(lists:nth(2, FieldList)),
        dis_name = to_list(lists:nth(3, FieldList)),
        show_pic = to_list(lists:nth(4, FieldList)),
        bullet_effect_path = to_list(lists:nth(5, FieldList)),
        bullet_effect_name = to_list(lists:nth(6, FieldList)),
        weapon_effect_path = to_list(lists:nth(7, FieldList)),
        weapon_effect_name = to_list(lists:nth(8, FieldList)),
        equip_effect_path = to_list(lists:nth(9, FieldList)),
        equip_effect = to_list(lists:nth(10, FieldList)),
        bullet_speed = to_int(lists:nth(11, FieldList), 0),
        can_attack = to_int(lists:nth(12, FieldList), 0),
        sound_event = to_list(lists:nth(13, FieldList)),
        type = to_int(lists:nth(14, FieldList), 0),
        max_level = to_int(lists:nth(15, FieldList), 0),
        star_level = to_int(lists:nth(16, FieldList), 1),
        belong_tower_type = to_int(lists:nth(17, FieldList), 0),
        upgrade_obj_id1 =  to_list(lists:nth(18, FieldList)),
        upgrade_obj_id2 = to_list(lists:nth(18, FieldList)),
        init_hp = to_int(lists:nth(20, FieldList), 1),
        hp_per_level = to_int(lists:nth(21, FieldList), 1),
        physical_attack_min = to_float(lists:nth(22, FieldList) , 0.0),
        physical_attack_min_per_level = to_float(lists:nth(23, FieldList), 0.0),
        physical_attack_max = to_float(lists:nth(24, FieldList), 0.0),
        physical_attack_max_per_level = to_float(lists:nth(25, FieldList), 0.0),
        magic_attack_min = to_float(lists:nth(26, FieldList), 0.0),
        magic_attack_min_per_level = to_float(lists:nth(27, FieldList), 0.0),
        magic_attack_max = to_float(lists:nth(28, FieldList), 0.0),
        magic_attack_max_per_level = to_float(lists:nth(29, FieldList),0.0),
        crit_attack_level = to_int(lists:nth(30, FieldList), 0),
        crit_attack_grow = to_float(lists:nth(31, FieldList), 0.0),
        hit_level = to_int(lists:nth(32, FieldList), 0),
        hit_grow = to_float(lists:nth(33, FieldList), 0.0),
        speed_level = to_int(lists:nth(34, FieldList), 0),
        speed_grow = to_float(lists:nth(35, FieldList), 0.0),
        skill_id = to_list(lists:nth(36, FieldList)),
        price = to_int(lists:nth(37, FieldList), 0),
        gain_exp = to_int(lists:nth(38, FieldList), 0),
        gain_exp_per_level = to_int(lists:nth(39, FieldList), 0),
        base_dsm = to_int(lists:nth(40, FieldList), 0),
        dsm_per_level = to_int(lists:nth(41, FieldList), 0),
        max_level_1 = to_int(lists:nth(42, FieldList), 0),
        base_score_addition = to_int(lists:nth(43, FieldList), 0),
        base_score_addition_growth = to_float(lists:nth(44, FieldList), 0.0),
        icon = to_list(lists:nth(45, FieldList)),
        advance_cost = to_int(lists:nth(46, FieldList) ,0),
        advance_material_1_id = to_list_restrict_length(lists:nth(47, FieldList)),
        advance_material_1_num = to_int(lists:nth(48, FieldList) ,0),
        advance_material_2_id = to_list_restrict_length(lists:nth(49, FieldList)),
        advance_material_2_num = to_int(lists:nth(50, FieldList), 0),
        advance_material_3_id = to_list_restrict_length(lists:nth(51, FieldList)),
        advance_material_3_num = to_int(lists:nth(52, FieldList), 0),
        advance_material_4_id = to_list_restrict_length(lists:nth(53, FieldList)),
        advance_material_4_num = to_int(lists:nth(54, FieldList), 0),
        advance_material_5_id = to_list_restrict_length(lists:nth(55, FieldList)),
        advance_material_5_num = to_int(lists:nth(56, FieldList), 0)},
        Value
%%         ets:insert(res_equip_config,  Value),
%%         ets:insert(res_drop_index_table, #res_drop_config{id = Value#res_equipment.id, type = 1})
    end, LineList),
	Time2 = dd_util:milliseconds(),
	io:format("now  in  load csv cost ms time is --->~p~n",[Time2-Time1]);

load_csv("TollgateDrop.csv", Path) ->
  FilePath = Path ++ "TollgateDrop.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),
  lists:map(
    fun(Line) ->
      FiledList = read_field(Line, [], []),
      Value = #res_tollgate_drop{
        id = to_list_restrict_length(lists:nth(1, FiledList)),
        treasure_id = to_int(lists:nth(2, FiledList), 0)
      },
      ets:insert(res_tollgate_drop, Value)
    end, LineList);

load_csv("EquipPiece.csv", Path) ->
    FilePath = Path ++ "EquipPiece.csv",
    {ok, F} = file:open(FilePath, read),
    LineList = read_csv_line(F, []),
    file:close(F),
    lists:map(
      fun(Line) ->
        FiledList = read_field(Line, [], []),
        Value = #res_equip_piece{
          id = to_list_restrict_length(lists:nth(1, FiledList)),
          name = to_list(lists:nth(2, FiledList)),
          star = to_int(lists:nth(4, FiledList), 0),
          evolve_id = to_list_restrict_length(lists:nth(5, FiledList)),
          evolve_count = to_int(lists:nth(6, FiledList), 0)
        },
        ets:insert(res_equip_piece, Value),
        ets:insert(res_drop_index_table, #res_drop_config{id = Value#res_equip_piece.id, type = 5})
      end, LineList);

load_csv("InscriptionPiece.csv", Path) ->
  FilePath = Path ++ "InscriptionPiece.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),
  lists:map(
    fun(Line) ->
      FiledList = read_field(Line, [], []),
      Value = #res_inscription_piece{
        id = to_list_restrict_length(lists:nth(1, FiledList)),
        name = to_list(lists:nth(2, FiledList)),
        star = to_int(lists:nth(4, FiledList), 0),
        evolve_inscription_id = to_list(lists:nth(5, FiledList)),
        belong_tower = to_list(lists:nth(7, FiledList))
      },
      ets:insert(res_inscription_piece, Value),
      ets:insert(res_drop_index_table, #res_drop_config{id = Value#res_inscription_piece.id, type = 6})
    end, LineList);

load_csv("Inscription.csv", Path) ->
  FilePath = Path ++ "Inscription.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),
  lists:map(
    fun(Line) ->
      FiledList = read_field(Line, [], []),
      Value = #res_inscription{
        id = to_list_restrict_length(lists:nth(1, FiledList)),
        belong_tower = to_list(lists:nth(3, FiledList)),
        evolve_id = to_list(lists:nth(6, FiledList)),
        evol_need_piece = to_list(lists:nth(7, FiledList)),
        evol_need_count = to_int(lists:nth(8, FiledList), 0),
        compose_piece = to_list(lists:nth(9, FiledList)),
        compose_count = to_int(lists:nth(10, FiledList), 0),
        star = to_int(lists:nth(11, FiledList), 0),
        type = to_int(lists:nth(12, FiledList), 0),
        inscription_class = to_list(lists:nth(40, FiledList))
      },
      ets:insert(res_inscription, Value),
      ets:insert(res_drop_index_table, #res_drop_config{id = Value#res_inscription.id, type = 2})
    end, LineList);

load_csv("Tower.csv", Path) ->
    FilePath = Path ++ "Tower.csv",
    {ok, F} = file:open(FilePath, read),
    LineList = read_csv_line(F, []),
    file:close(F),
    lists:map(
      fun(Line) ->
        FiledList = read_field(Line, [], []),
        Value = #res_tower{
          id = to_list(lists:nth(1, FiledList)),
          name = to_list(lists:nth(2, FiledList)),
          level = to_int(lists:nth(3, FiledList), 0),
          type = to_int(lists:nth(4, FiledList), 0),
          base_weapon_id = to_list(lists:nth(19, FiledList)),
          base_second_weapon_id = to_list(lists:nth(20, FiledList)),
          base_hat_id = to_list(lists:nth(21, FiledList))
        },
        ets:insert(res_tower_config, Value)
      end, LineList);

  load_csv("Material.csv", Path) ->
    FilePath = Path ++ "Material.csv",
    {ok, F} = file:open(FilePath, read),
    LineList = read_csv_line(F, []),
    file:close(F),


    lists:map(
      fun(Line) ->
        FiledList = read_field(Line, [], []),
        Value = #res_material{
          id = to_list(lists:nth(1, FiledList)),
          name = to_list(lists:nth(2, FiledList)),
          src = to_list(lists:nth(3, FiledList)),
          star_level = to_int(lists:nth(4, FiledList), 0),
          price = to_int(lists:nth(5, FiledList), 0),
          gain_exp = to_int(lists:nth(6, FiledList), 0),
          evolv_id = to_list(lists:nth(7, FiledList)),
          evolv_count = to_int(lists:nth(8, FiledList), 0),
          desc = to_list(lists:nth(9, FiledList))
        },
        ets:insert(res_material_config, Value),
        ets:insert(res_drop_index_table, #res_drop_config{id = Value#res_material.id, type = 3})
      end, LineList);


 load_csv("InitialAccount.csv", Path) ->
   FilePath = Path ++ "InitialAccount.csv",
   {ok, F} = file:open(FilePath, read),
   LineList = read_csv_line(F, []),
   file:close(F),

   lists:map(
     fun(Line) ->
       FiledList = read_field(Line, [], []),
       Value = #res_init_account{
         id = to_int(lists:nth(1, FiledList), 0),
         init_gold = to_int(lists:nth(2, FiledList), 0),
         init_gem = to_int(lists:nth(3, FiledList), 0),
         init_strength = to_int(lists:nth(4, FiledList), 0),
         init_backpack_capacity = to_int(lists:nth(5, FiledList), 0),
         init_heros = string:tokens(to_list(lists:nth(6, FiledList)),";"),
         init_property = string:tokens(to_list(lists:nth(7, FiledList)),";"),
         init_material = string:tokens(to_list(lists:nth(8, FiledList)), ";"),
         init_select_hero = string:tokens(to_list(lists:nth(9, FiledList)), ";")
       },
       ets:insert(res_init_account, Value)
     end, LineList);

 load_csv("Tollgate.csv", Path) ->
   FilePath = Path ++ "Tollgate.csv",
   {ok, F} = file:open(FilePath, read),
   LineList = read_csv_line(F, []),
   file:close(F),
   load_csv("TollgateEnergy.csv", Path),

   lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_stage{
        id = to_int(lists:nth(1, FieldList), 0),
        desc = to_list(lists:nth(2, FieldList)),
        type = to_int(lists:nth(3, FieldList), 0),
        background_id = to_list(lists:nth(4, FieldList)),
        map_id = to_list(lists:nth(5, FieldList)),
        music_id = to_list(lists:nth(6, FieldList)),
        tower_select = string:tokens(to_list(lists:nth(7, FieldList)),";"),
        init_gold =  to_int(lists:nth(8, FieldList), 0),
        road = to_int(lists:nth(9, FieldList), 1),
        monster = string:tokens(to_list(lists:nth(10, FieldList)),";"),
        player_hp = to_int(lists:nth(11, FieldList), 0),
        exception_combat = to_int(lists:nth(12, FieldList), 0),
        drop_1 = to_list_restrict_length(lists:nth(13, FieldList)),
        drop_1_prob = to_int(lists:nth(14, FieldList), 0),
        drop_2 = to_list_restrict_length(lists:nth(15, FieldList)),
        drop_2_prob = to_int(lists:nth(16, FieldList), 0),
        drop_3 = to_list_restrict_length(lists:nth(17, FieldList)),
        drop_3_prob = to_int(lists:nth(18, FieldList), 0),
        boss_drop = to_list(lists:nth(19, FieldList)),
        cost_strength = to_int(lists:nth(20, FieldList), 0),
        pre_tollgate_id = to_int(lists:nth(21, FieldList), to_int(lists:nth(1, FieldList), 1) - 1),   %%如果为空
        tollgate_type = to_int(lists:nth(22, FieldList), 0),  %%关卡类型
        cool_time = to_int(lists:nth(23, FieldList), 0),
        gain_gold = to_int(lists:nth(24, FieldList), 0),
        daily_restrict_count = to_int(lists:nth(25, FieldList), 0)
      },
      NewValue = get_energy_cost(Value),
      ets:insert(res_stage, NewValue)
   end, LineList);

load_csv("TollgateEnergy.csv", Path) ->
  FilePath = Path ++ "TollgateEnergy.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),
  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      TollgateType = to_int(lists:nth(2, FieldList), 0),
      EnergyCostType = to_int(lists:nth(3, FieldList), 0),
      SeqNum = to_int(lists:nth(4, FieldList), 0),
      EnergyCost = to_int(lists:nth(5, FieldList), 0),
      EnergySweepCost = to_int(lists:nth(6, FieldList), 0),
      case ets:lookup(res_tollgate_energy, TollgateType) of
        [] ->
          case EnergyCostType of
            1 ->
              TollgateEnergy1 = #res_tollgate_energy{tollgate_type = TollgateType, normal_energy = [{SeqNum, EnergyCost}], addition_energy = [], sweep_normal_energy = [{SeqNum, EnergySweepCost}], sweep_addition_energy = []},
              ets:insert(res_tollgate_energy, TollgateEnergy1);
            2 ->
              TollgateEnergy2 = #res_tollgate_energy{tollgate_type = TollgateType, addition_energy = [{SeqNum, EnergyCost}], normal_energy = [], sweep_addition_energy = [{SeqNum, EnergySweepCost}], sweep_normal_energy = []},
              ets:insert(res_tollgate_energy, TollgateEnergy2)
          end;
        [TE] ->
          case EnergyCostType of
            1 ->
              NTE1 = TE#res_tollgate_energy{normal_energy = [{SeqNum, EnergyCost} | TE#res_tollgate_energy.normal_energy], sweep_normal_energy = [{SeqNum, EnergySweepCost} | TE#res_tollgate_energy.sweep_normal_energy]},
              ets:insert(res_tollgate_energy, NTE1);
            2 ->
              NTE2 = TE#res_tollgate_energy{addition_energy = [{SeqNum, EnergyCost} | TE#res_tollgate_energy.addition_energy], sweep_addition_energy = [{SeqNum, EnergySweepCost} | TE#res_tollgate_energy.sweep_addition_energy]},
              ets:insert(res_tollgate_energy, NTE2)
          end
      end
    end, LineList);

load_csv("WorldMapBlock.csv", Path) ->
  FilePath = Path ++ "WorldMapBlock.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_world_map_block{
        id = to_list(lists:nth(1, FieldList)),
        path = to_list(lists:nth(2, FieldList)),
        armature = to_list(lists:nth(3, FieldList)),
        lock_animation = to_list(lists:nth(4, FieldList)),
        unlock_animation = to_list(lists:nth(5, FieldList)),
        cd_animation = to_list(lists:nth(6, FieldList)),
        unlock_action = to_list(lists:nth(7, FieldList)),
        bind_node = to_list(lists:nth(8, FieldList)),
        interaction_type = to_int(lists:nth(9, FieldList), 0),
        unlock_tollgate = to_int(lists:nth(10, FieldList), 0),
        materials = string:tokens(to_list(lists:nth(11, FieldList)), ";")
      },
      ets:insert(res_world_map_block, Value)
    end, LineList);

load_csv("ExpTable.csv", Path) ->
  FilePath = Path ++ "ExpTable.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_exp_config{
        id = to_int(lists:nth(1, FieldList), 0),
        need_exp = to_int(lists:nth(2, FieldList), 0),
        total_exp = to_int(lists:nth(3, FieldList), 0)
      },
      ets:insert(res_exp_config, Value)
    end, LineList);

load_csv("DailyLoginReward.csv", Path) ->
  FilePath = Path ++ "DailyLoginReward.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_login_reward{
        id = to_int(lists:nth(1, FieldList), 0),
        reward_type = to_int(lists:nth(2, FieldList), 0),
        reward_number =  to_int(lists:nth(3, FieldList), 0)
      },
      ets:insert(res_login_reward_config, Value)
    end, LineList);

load_csv("Task.csv", Path) ->
  FilePath = Path ++ "Task.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_task{
        id = to_list(lists:nth(1, FieldList)),
        task_type = to_int(lists:nth(2, FieldList), 0),
        type_param_1 = to_int(lists:nth(3, FieldList), 0),
        type_param_2 = to_list(lists:nth(4, FieldList)),
        type_param_3 = to_list(lists:nth(5, FieldList)),
        task_name = to_list(lists:nth(6, FieldList)),
        task_desc = to_list(lists:nth(7, FieldList)),
        reward_type = to_int(lists:nth(8, FieldList), 0),
        reward_property_id = to_list(lists:nth(9, FieldList)),
        reward_num = to_int(lists:nth(10, FieldList), 0),
        condition_1 = to_int(lists:nth(11, FieldList), 0),
        condition_2 = to_int(lists:nth(12, FieldList), 0),
        daily_limit = to_int(lists:nth(13, FieldList), 0),
        daily_refresh = to_int(lists:nth(14, FieldList), 0),
        start_ts = time_to_ts(lists:nth(15, FieldList), {0,0,0}),
        over_ts = time_to_ts(lists:nth(16, FieldList), {0,0,0})
      },
      ets:insert(res_task_config, Value)
    end, LineList);

load_csv("Achievement.csv", Path) ->
  FilePath = Path ++ "Achievement.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_achievement{
        id = to_list(lists:nth(1, FieldList)),
        type_param_1 = to_int(lists:nth(2, FieldList), 0),
        group_id = to_int(lists:nth(3, FieldList), 0),
        level = to_int(lists:nth(4, FieldList), 0),
        type_param_2 = to_list(lists:nth(5, FieldList)),
        type_param_3 = to_list(lists:nth(6, FieldList)),
        share_param = to_int(lists:nth(7, FieldList), 0),
        name = to_list(lists:nth(8, FieldList)),
        desc = to_list(lists:nth(9, FieldList)),
        reward_type = to_int(lists:nth(10, FieldList), 0),
        reward_property_id = to_list(lists:nth(11, FieldList)),
        reward_num = to_int(lists:nth(12, FieldList), 0),
        condition_1 = to_int(lists:nth(13, FieldList), 0),
        condition_2 = to_int(lists:nth(14, FieldList), 0)
      },
      ets:insert(res_achievement_config, Value)
    end, LineList);

load_csv("Shop.csv", Path) ->
  FilePath = Path ++ "Shop.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_goods{
        id = to_list(lists:nth(1, FieldList)),
        name = to_list(lists:nth(2, FieldList)),
        pic = to_list(lists:nth(3, FieldList)),
        goods_no = to_list(lists:nth(4, FieldList)),
        goods_type = to_int(lists:nth(5, FieldList), 0),
        goods_count = to_int(lists:nth(6, FieldList), 0),
        money_type = to_int(lists:nth(7, FieldList), 0),
        money_count = to_list(lists:nth(8, FieldList)),
        original_price = to_list(lists:nth(9, FieldList)),
        discount = to_list(lists:nth(10, FieldList)),
        gift_type = to_int(lists:nth(11, FieldList), 0),
        gift_id = to_list(lists:nth(12, FieldList)),
        gift_count = to_int(lists:nth(13, FieldList), 0),
        restrict_count = to_int(lists:nth(14, FieldList), 0),
        refresh_type = to_int(lists:nth(15, FieldList), 0),
        is_recommend = to_int(lists:nth(16, FieldList), 0),
        start_ts = time_to_ts(lists:nth(17, FieldList), {0,0,0}),
        over_ts = time_to_ts(lists:nth(18, FieldList), {0,0,0}),
        promotion_type = to_int(lists:nth(19, FieldList), 0),
        view_switch = to_int(lists:nth(20, FieldList), 0)
      },
      ets:insert(res_shop_config, Value)
    end, LineList);

load_csv("ShopPackage.csv", Path) ->
  FilePath = Path ++ "ShopPackage.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_shop_package{
        package_id = to_list(lists:nth(1, FieldList)),
        package_name = to_list(lists:nth(2, FieldList)),
        package_1_type = to_int(lists:nth(3, FieldList), 0),
        package_1_id = to_list(lists:nth(4, FieldList)),
        package_1_count = to_int(lists:nth(5, FieldList), 0),
        package_2_type = to_int(lists:nth(6, FieldList), 0),
        package_2_id = to_list(lists:nth(7, FieldList)),
        package_2_count = to_int(lists:nth(8, FieldList), 0),
        package_3_type = to_int(lists:nth(9, FieldList), 0),
        package_3_id = to_list(lists:nth(10, FieldList)),
        package_3_count = to_int(lists:nth(11, FieldList), 0),
        package_4_type = to_int(lists:nth(12, FieldList), 0),
        package_4_id = to_list(lists:nth(13, FieldList)),
        package_4_count = to_int(lists:nth(14, FieldList), 0),
        package_5_type = to_int(lists:nth(15, FieldList), 0),
        package_5_id = to_list(lists:nth(16, FieldList)),
        package_5_count = to_int(lists:nth(17, FieldList), 0)
      },
      ets:insert(res_shop_package, Value)
    end, LineList);

load_csv("ActivePointReward.csv", Path) ->
  FilePath = Path ++ "ActivePointReward.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_activity_reward{
        id = to_int(lists:nth(1, FieldList), 0),
        reward_type = to_int(lists:nth(2, FieldList), 0),
        reward_property_id = to_list(lists:nth(3, FieldList)),
        reward_num = to_int(lists:nth(4, FieldList), 0)
      },
      ets:insert(res_activity_reward_config, Value)
    end, LineList);

load_csv("Tools.csv", Path) ->
  FilePath = Path ++ "Tools.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_property{
        id = to_list(lists:nth(1, FieldList)),
        name = to_list(lists:nth(2, FieldList)),
        type = to_int(lists:nth(3, FieldList), 0)
      },
      ets:insert(res_property_config, Value),
      ets:insert(res_drop_index_table, #res_drop_config{id = Value#res_property.id, type = 4})
    end, LineList);

load_csv("Mail.csv", Path) ->
  FilePath = Path ++ "Mail.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_template_mail_config{
        id = to_list(lists:nth(1, FieldList)),
        type = to_int(lists:nth(2, FieldList), 1),
        tag = to_list(lists:nth(3, FieldList)),
        title = to_list(lists:nth(4, FieldList)),
        content = get_mail_content(to_list(lists:nth(5, FieldList))),
        attach_type = to_int(lists:nth(6, FieldList), 0),
        tool_id = to_list(lists:nth(7, FieldList)),
        attach_num = to_int(lists:nth(8, FieldList), 0),
        term = to_int(lists:nth(9, FieldList), 7)
      },
      ets:insert(res_template_mail_config, Value)
    end, LineList);

load_csv("Language.csv", Path) ->
  FilePath = Path ++ "Language.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_language_config{
        id = to_list(lists:nth(1, FieldList)),
        chinese = to_list(lists:nth(2, FieldList)),
        english = to_list(lists:nth(3, FieldList))
      },
      ets:insert(res_language_config, Value)
    end, LineList);

load_csv("TreasureConfig.csv", Path) ->
  FilePath = Path ++ "TreasureConfig.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      GroupId = to_int(lists:nth(1, FieldList), 0),
      Item = #res_treasure_item_config{
        sub_drop_id = to_int(lists:nth(2, FieldList), 0),
        item_id = to_list(lists:nth(3, FieldList)),
        item_name = to_list(lists:nth(4, FieldList)),
        item_type = to_int(lists:nth(5, FieldList), 0),
        item_count = to_int(lists:nth(6, FieldList), 0),
        item_probability = to_int(lists:nth(7, FieldList), 0),
        item_level = to_int(lists:nth(8, FieldList), 1)
      },
      case ets:lookup(res_treasure_config, GroupId) of
        [] ->
          GroupItem = #res_treasure_config{
            group_id = GroupId,
            item_list = [Item]
          },
          ets:insert(res_treasure_config, GroupItem);
        [Group] ->
          NGroup = Group#res_treasure_config{item_list = [Item | Group#res_treasure_config.item_list]},
          ets:insert(res_treasure_config, NGroup)
      end
    end, LineList),
  update_chest_prop_value();

load_csv("EndlessConfig.csv", Path) ->
  FilePath = Path ++ "EndlessConfig.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_endless_config{
        wave_id = to_int(lists:nth(1, FieldList), 0),
        hp_coefficients = to_float(lists:nth(2, FieldList), 0.0),
        drop_chest_id = to_int(lists:nth(3, FieldList), 0),
        drop_probability = to_int(lists:nth(4, FieldList), 0)
      },
      ets:insert(res_endless_config, Value)
    end, LineList);

load_csv("Lottery.csv", Path) ->
  FilePath = Path ++ "Lottery.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Type = to_int(lists:nth(1, FieldList), 0),
      Item = #res_lottery_item_config{
        lottery_id = to_int(lists:nth(2, FieldList), 0),
        lottery_prob = to_int(lists:nth(3, FieldList), 0),
        lottery_limit = to_int(lists:nth(4, FieldList), 0)
      },
      case ets:lookup(res_lottery_config, Type) of
        [] ->
          Lottery = #res_lottery_config{
            lottery_type = Type,
            lottery_item_lists = [Item]
          },
          ets:insert(res_lottery_config, Lottery);
        [LotteryGroup] ->
          NGroup = LotteryGroup#res_lottery_config{lottery_item_lists = [Item | LotteryGroup#res_lottery_config.lottery_item_lists]},
          ets:insert(res_lottery_config, NGroup)
      end
    end, LineList),
  update_lottery_prop_value();

load_csv("Treasure.csv", Path) ->
  FilePath = Path ++ "Treasure.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_lottery_shop_config{
        lottery_id = to_int(lists:nth(1, FieldList), 0),
        sale_id = to_list(lists:nth(2, FieldList)),
        price = to_int(lists:nth(3, FieldList), 0),
        date = to_list(lists:nth(4, FieldList))
      },
      ets:insert(res_lottery_shop_config, Value)
    end, LineList);

%% load_csv("Monster.csv", Path) ->
%%   FilePath = Path ++ "Monster.csv",
%%   {ok, F} = file:open(FilePath, read),
%%   LineList = read_csv_line(F, []),
%%   file:close(F),
%%   lists:map(
%%     fun(Line) ->
%%       FieldList = read_field(Line, [], []),
%%       Value = #monster{
%%         id = to_list(lists:nth(1, FieldList)),
%%         type = to_list(lists:nth(3, FieldList)),
%%         drop = to_list(lists:nth(34, FieldList))
%%       },
%%       ets:insert(res_monster, Value)
%%     end, LineList);


%% load_csv("MonsterGroup.csv", Path) ->
%%   FilePath = Path ++ "MonsterGroup.csv",
%%   {ok, F} = file:open(FilePath, read),
%%   LineList = read_csv_line(F, []),
%%   file:close(F),
%%
%%   lists:map(
%%     fun(Line) ->
%%       FieldList = read_field(Line, [], []),
%%       Value = #monster_group{
%%         group_id = to_list(lists:nth(1, FieldList)),
%%         monster_id_1 = to_list(lists:nth(3, FieldList)),
%%         monster_id_2 = to_list(lists:nth(8, FieldList)),
%%         monster_id_3 = to_list(lists:nth(13, FieldList))
%%       },
%%       ets:insert(res_monster_group, Value)
%%     end, LineList);

load_csv("ContinuousLoginReward.csv", Path) ->
  FilePath = Path ++ "ContinuousLoginReward.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_continuous_login_reward{
        id = to_int(lists:nth(1, FieldList), 0),
        reward_player_type = to_int(lists:nth(2, FieldList), 0),
        reward_type = to_int(lists:nth(3, FieldList), 0),
        reward_deliver_type = to_int(lists:nth(4, FieldList), 0),
        reward_prop_id = to_list(lists:nth(5, FieldList)),
        reward_count = to_int(lists:nth(6, FieldList), 0),
        reward_desc = to_list(lists:nth(7, FieldList)),
        reward_start_ts = time_to_ts(lists:nth(8, FieldList), {0,0,0}),
        reward_over_ts = time_to_ts(lists:nth(9, FieldList), {0,0,0})
      },
      ets:insert(res_continuous_login_reward, Value)
    end, LineList);

load_csv("EquipUpgradeCost.csv", Path) ->
  FilePath = Path ++ "EquipUpgradeCost.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),

  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_equip_upgrade_cost{
        id = to_int(lists:nth(1, FieldList), 0),
        main_cost = to_int(lists:nth(2, FieldList), 0),
        main_total = to_int(lists:nth(3, FieldList), 0),
        off_cost = to_int(lists:nth(4, FieldList), 0),
        off_total = to_int(lists:nth(5, FieldList), 0),
        head_cost = to_int(lists:nth(6, FieldList), 0),
        head_total = to_int(lists:nth(7, FieldList), 0)
      },
      ets:insert(res_equip_upgrade_cost, Value)
    end, LineList);

load_csv("CDKeyPackageConfig.csv", Path) ->
  FilePath = Path ++ "CDKeyPackageConfig.csv",
  {ok, F} = file:open(FilePath, read),
  LineList = read_csv_line(F, []),
  file:close(F),
  lists:map(
    fun(Line) ->
      FieldList = read_field(Line, [], []),
      Value = #res_cdkey_package_config{
        id = to_list(lists:nth(1, FieldList)),
        type = to_int(lists:nth(2, FieldList), 0),
        spec_cdkey = to_list(lists:nth(3, FieldList)),
        name = to_list(lists:nth(4, FieldList)),
        c1_type = to_int(lists:nth(5, FieldList),0),
        c1_id = to_list(lists:nth(6, FieldList)),
        c1_count = to_int(lists:nth(7, FieldList), 0),
        c2_type = to_int(lists:nth(8, FieldList),0),
        c2_id = to_list(lists:nth(9, FieldList)),
        c2_count = to_int(lists:nth(10, FieldList), 0),
        c3_type = to_int(lists:nth(11, FieldList),0),
        c3_id = to_list(lists:nth(12, FieldList)),
        c3_count = to_int(lists:nth(13, FieldList), 0),
        c4_type = to_int(lists:nth(14, FieldList),0),
        c4_id = to_list(lists:nth(15, FieldList)),
        c4_count = to_int(lists:nth(16, FieldList), 0),
        c5_type = to_int(lists:nth(17, FieldList),0),
        c5_id = to_list(lists:nth(18, FieldList)),
        c5_count = to_int(lists:nth(19, FieldList), 0)
      },
      ets:insert(res_cdkey_package_config, Value)
    end, LineList).

get_energy_cost(TollgateConfig) ->
  TollgateID = TollgateConfig#res_stage.id,
  if
    TollgateID > 10000 -> TollgateConfig;
    true ->
      case ets:lookup(res_tollgate_energy, TollgateConfig#res_stage.type) of
        [] ->
          throw({custom, "HintSystemDataError"});
        [TollgateEnergy] ->
          NormalCount = length(TollgateEnergy#res_tollgate_energy.normal_energy),
          AdditionCount = length(TollgateEnergy#res_tollgate_energy.addition_energy),
          TollgateConfig#res_stage{
            daily_restrict_count = NormalCount,
            daily_addition_count = AdditionCount,
            normal_cost_energy = TollgateEnergy#res_tollgate_energy.normal_energy,
            normal_sweep_energy = TollgateEnergy#res_tollgate_energy.sweep_normal_energy,
            addition_cost_energy = TollgateEnergy#res_tollgate_energy.addition_energy,
            addition_sweep_energy = TollgateEnergy#res_tollgate_energy.sweep_addition_energy
          }
      end
  end.


update_chest_prop_value() ->
  List = ets:tab2list(res_treasure_config),
  lists:foreach(
    fun(DropConfigItem) ->
      {ItemList, _} = lists:foldl(
        fun(Item, {TmpList, TmpLow}) ->
          Prob = Item#res_treasure_item_config.item_probability,
          NItem = Item#res_treasure_item_config{prob_low_value = TmpLow, prob_up_value = (TmpLow + Prob - 1)},
          {[NItem | TmpList], TmpLow + Prob}
        end, {[], 0}, DropConfigItem#res_treasure_config.item_list),
      NDropConfigItem = DropConfigItem#res_treasure_config{item_list = ItemList},
      ets:insert(res_treasure_config, NDropConfigItem)
    end, List).

update_lottery_prop_value() ->
  List = ets:tab2list(res_lottery_config),
  lists:foreach(
    fun(LotteryItem) ->
      {ItemList, _} = lists:foldl(
        fun(Item, {TmpList, TmpLow}) ->
          Prob = Item#res_lottery_item_config.lottery_prob,
          NItem = Item#res_lottery_item_config{prob_low_value = TmpLow, prob_up_value = (TmpLow + Prob - 1)},
          {[NItem | TmpList], TmpLow + Prob}
        end, {[], 0}, LotteryItem#res_lottery_config.lottery_item_lists),
      NConfigItem = LotteryItem#res_lottery_config{lottery_item_lists = ItemList},
      ets:insert(res_lottery_config, NConfigItem)
    end, List).

get_mail_content(Id) when is_list(Id) ->
  case ets:lookup(res_language_config, Id) of
    [] -> "";
    [LanguageConfig] -> LanguageConfig#res_language_config.chinese
  end.

%% load_monster_wave_csv(FilePath) ->
%%   case file:open(FilePath, read) of
%%     {ok, F} ->
%%       LineList = read_csv_line(F, []),
%%       file:close(F),
%%       lists:map(
%%         fun(Line) ->
%%           FieldList = read_field(Line, [], []),
%%           to_list(lists:nth(1, FieldList))  %怪物波ID
%%         end, LineList);
%%     _ ->
%%       []
%%   end.
%%
%% get_tollgate_config_name(TollgateID) when is_integer(TollgateID) ->
%%     if
%%       TollgateID < 10 -> "00" ++ dd_util:to_list(TollgateID) ++ "WaveMonster0.csv";
%%       TollgateID < 100 -> "0" ++ dd_util:to_list(TollgateID) ++ "WaveMonster0.csv";
%%       TollgateID < 10000 -> dd_util:to_list(TollgateID) ++ "WaveMonster0.csv";
%%       TollgateID < 20000 ->
%%         Num = TollgateID-10000,
%%         Name =
%%           if
%%             Num < 10 -> "00"++dd_util:to_list(Num);
%%             Num < 100 -> "0"++dd_util:to_list(Num);
%%             true -> dd_util:to_list(Num)
%%           end,
%%         "inf" ++ Name ++ "WaveMonster0.csv";
%%       true ->  dd_util:to_list(TollgateID) ++ "WaveMonster0.csv"
%%     end.
%%
%% get_all_drops(TollgateID, NormalDropList, BossDropList, Path) when is_integer(TollgateID) andalso is_list(NormalDropList) andalso is_list(BossDropList) ->
%%   if
%%     TollgateID < 20000 andalso TollgateID > 10000 -> [];
%%     true ->
%%       CurConfigName = get_tollgate_config_name(TollgateID),
%%       WaveConfigName = Path ++ "tollgate/" ++CurConfigName,
%%       List0 = load_monster_wave_csv(WaveConfigName),  %怪物ID列表
%%       if
%%         List0 =:= [] ->     %%怪物波ID为空，或没有该文件
%% %          ?FILE_LOG_DEBUG("there is no WaveMonster0.csv file,TollgateID is ~p",[TollgateID]),
%%           NormalDropList;
%%         true ->
%%           SumDropList =
%%             lists:map(
%%               fun(MonsterWave_id) ->
%%                 case ets:lookup(res_monster_group,MonsterWave_id) of
%%                   [] -> [];
%%                   [Monster_group] ->
%%                     Monster_id_list =
%%                       [
%%                         Monster_group#monster_group.monster_id_1,
%%                         Monster_group#monster_group.monster_id_2,
%%                         Monster_group#monster_group.monster_id_3
%%                       ],
%%                     EachDrop =
%%                       lists:map(
%%                         fun(Monster_id) ->
%%                           case Monster_id of
%%                             [] -> [];
%%                             _Other ->
%%                               [Item] = ets:lookup(res_monster,Monster_id),
%%                               Drop = Item#monster.drop,
%%                               Drop0 =
%%                                 case Drop of
%%                                   [] -> [];
%%                                   Other -> list_to_atom(Other)
%%                                 end,
%%                               Drop0
%%                           end
%%                         end,Monster_id_list),
%%                     EachDrop
%%                 end
%%               end, List0),
%%           SumDropList0 = lists:flatten(SumDropList),
%%           case SumDropList0 of
%%             [] ->
%% %              ?FILE_LOG_DEBUG("TotalDrop is ~p",[NormalDropList]),
%%               NormalDropList;
%%             _ ->
%%               SumDropList1 = lists:map(fun(X)->dd_util:to_list(X) end,SumDropList0),
%%               TotalDrop = lists:merge(SumDropList1,NormalDropList),
%% %              ?FILE_LOG_DEBUG("TotalDrop is ~p",[TotalDrop]),
%%               TotalDrop
%%           end
%%       end
%%   end.





