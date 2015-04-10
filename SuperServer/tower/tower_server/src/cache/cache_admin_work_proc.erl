%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2014 下午3:19
%%%-------------------------------------------------------------------
-module(cache_admin_work_proc).
-author("zqlt").

-include("cache_def.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-include("../admin/adminserver.hrl").
-include("../csv.hrl").

%% API56
-export([execute/4]).
-export([execute/5]).
%% return {success, NAccount, FieldList}


execute(add_gem, _Uin, Data, Account,Oper) ->    %Action,Uin,Param,Account
  ?FILE_LOG_DEBUG("execute add_gem ", []),
  case Oper of
    1 ->
      G = Account#account.gem,
      NAccount = Account#account{gem = G + dd_util:to_integer(get_json_value(<<"value">>,Data))},
      {success,NAccount,["gem"]};
    0 ->
      G = Account#account.gem,
      N = dd_util:to_integer(get_json_value(<<"value">>,Data)),
      ?FILE_LOG_DEBUG("G = ~p, N = ~p",[G,N]),
      if
        G > N ->
          NAccount = Account#account{gem = G - N},
          {success,NAccount,["gem"]};
        true ->
          {fail,"gem not enough"}
      end
  end;


execute(add_money, _Uin, Data, Account, Oper) ->
  ?FILE_LOG_DEBUG("execute add_money ", []),
  case Oper of
    1 ->
      G = Account#account.gold_coin,
      NAccount = Account#account{gold_coin = G + dd_util:to_integer(get_json_value(<<"value">>,Data))},
      {success,NAccount,["gold_coin"]};
    0 ->
      G = Account#account.gold_coin,
      N = dd_util:to_integer(get_json_value(<<"value">>,Data)),
      ?FILE_LOG_DEBUG("G = ~p, N = ~p",[G,N]),
      if
        G > N ->
          NAccount = Account#account{gold_coin = G - N},
          {success,NAccount,["gold_coin"]};
        true ->
          {fail,"gold_coin not enough"}
      end
  end;

execute(add_strength, _Uin, Data, Account, Oper) ->
  ?FILE_LOG_DEBUG("execute add_strength ", []),
  case Oper of
    1 ->
      G = Account#account.strength#strength.strength,
      NG = Account#account.strength#strength{strength = G + dd_util:to_integer(get_json_value(<<"value">>,Data))},
      NAccount = Account#account{strength = NG},
      {success,NAccount,["strength"]};
    0 ->
      G = Account#account.strength#strength.strength,
      N = dd_util:to_integer(get_json_value(<<"value">>,Data)),
      ?FILE_LOG_DEBUG("G = ~p, N = ~p",[G,N]),
      if
        G > N ->
          NG = Account#account.strength#strength{strength = G - dd_util:to_integer(get_json_value(<<"value">>,Data))},
          NAccount = Account#account{strength = NG},
          {success,NAccount,["strength"]};
        true ->
          {fail,"strength not enough"}
      end
  end.


%%GM接口可以给网页平台以及手机的debug端去调用
execute(add_backpack_player_piece, Uin, PlayerPieceList, Account) when is_list(PlayerPieceList) andalso is_record(Account, account) ->
  NPlayerPieceList =
    lists:map(
      fun({Id,Count}) ->
        #itemBaseInfo{itemType = _Type} = cache_csv:get_item_base_info(Id),
		UUid = cache_guid:alloc_guid(Uin),
        {uuid = UUid,id = Id, count = Count}
      end, PlayerPieceList),
  NPieceTree = cache_api:inc_tree_element(NPlayerPieceList, Account#account.backpack#backpack.s_fragment_list),
  NBackpack = Account#account.backpack#backpack{s_fragment_list = NPieceTree},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};


execute(get_user_info, Uin,Param_type,Account)when is_integer(Uin) andalso is_list(Param_type) ->
  ?FILE_LOG_DEBUG("cache_admin_work_proc=>get_user_info", []),
  ValueList =
    lists:map(
      fun(Type) ->
        Value =
          case Type of
            "player" ->
                Player_Data=#admin_player_data{
                  name = Account#account.platform_info#platform_info.player_dis_name,
                  uin = Account#account.uin,
                  plat = Account#account.platform_info#platform_info.plat_type ,                      %%注册来源
                  register_date = dd_util:time_format(dd_util:to_local_time(dd_util:timestamp_to_datetime(Account#account.create_ts))),           %%注册时间
                  atk = get_user_atk(Account),                %%战斗力
                  gem = Account#account.gem,               %%宝石
                  bind_gem = 0,
                  gold = Account#account.gold_coin,
                  strength = Account#account.strength#strength.strength,
                  tollgate_count = length(Account#account.stage#stage.base_tollgate_list),     %%当前关卡
                  total_star = lists:foldl(fun(Item,Sum)->Item#tollgate.max_star + Sum end, 0, Account#account.stage#stage.base_tollgate_list),         %%总星数
                  endless_max_score = Account#account.stage#stage.endless_tollgate#endless_tollgate.max_score,  %%无尽模式最高分数
                  endless_max_count = Account#account.stage#stage.endless_tollgate#endless_tollgate.max_wave_count,  %%无尽模式最高波次
                  pay_count= dd_util:float_to_string(Account#account.shop#shop.pay_info#pay_info.total_pay_val),           %%充值金额
                  pay_gem_count= Account#account.shop#shop.pay_info#pay_info.total_gem_count        %%充值宝石
                },
              ?FILE_LOG_DEBUG("account stage = ~p", [Account#account.stage]),
                Player_Data1=
                  [
                  {<<"dis_name">>, dd_util:to_binary(Player_Data#admin_player_data.name)},
                  {<<"plat">>,dd_util:to_binary(Player_Data#admin_player_data.plat)},
                  {<<"register_data">>,dd_util:to_binary(Player_Data#admin_player_data.register_date)},
                  {<<"atk">>,Player_Data#admin_player_data.atk},
                  {<<"gold">>, Player_Data#admin_player_data.gold},
                  {<<"gem">>, Player_Data#admin_player_data.gem},
                  {<<"bind_gem">>,Player_Data#admin_player_data.bind_gem},
                  {<<"strength">>, Player_Data#admin_player_data.strength},
                  {<<"tollgate_count">>,Player_Data#admin_player_data.tollgate_count},
                  {<<"total_star">>,Player_Data#admin_player_data.total_star},
                  {<<"endless_max_score">>,Player_Data#admin_player_data.endless_max_score},
                  {<<"endless_max_count">>,Player_Data#admin_player_data.endless_max_count},
                  {<<"pay_count">>,dd_util:to_binary(Player_Data#admin_player_data.pay_count)},
                  {<<"pay_gem_count">>,Player_Data#admin_player_data.pay_gem_count}
                ],
                Player_Data1;
            "tower" -> get_user_tower_info(Account);
            "tollgate" -> Account#account.stage;
            "backpack" -> get_backpack(Account);
            %%"backpack" -> Account#account.backpack;
			"reward_match" -> Account#account.reward_match;
            "mission" -> Account#account.mission;
            "achievement" -> Account#account.achievement;
            "shop" -> Account#account.shop;
            "guide_step" -> Account#account.addition#addition.newer_guide_steps;
            "gift_close" -> Account#account.strength#strength.close_strength_gift;
            "rank_reward_show" -> Account#account.addition#addition.rank_reward_got;
            "strength_buy_times" -> Account#account.strength#strength.today_buy_times;
            _ ->
              ?FILE_LOG_DEBUG("unknow type [~p]", [Type]),
              throw({custom, "HintRequestDataError"})
          end,
        {Type, Value}
      end, Param_type),
  {success,ValueList};



execute(reset_everyday_sign, Uin,_Data,Account)when is_integer(Uin) ->                   %重置每日签到  [reset_everyday_sign,Uin,0]
  ?FILE_LOG_DEBUG("cache_admin_work_proc=>reset_everyday_sign", []),
  Login_reward = Account#account.login_reward#login_reward{login_reward_list = [],login_times = 0},%#login_reward{latest_login_ts =  Account#account.login_reward#login_reward.latest_login_ts, login_reward_list = [],login_times = 0},
  NAccount = Account#account{login_reward = Login_reward},
  {success,NAccount,["login_reward"]};

execute(set_strength_buytimes, Uin,_Data,Account)when is_integer(Uin) ->                   %重置体力购买次数
  ?FILE_LOG_DEBUG("cache_admin_work_proc=>set_strength_buytimes", []),
  Strength = Account#account.strength#strength{today_buy_times = 0},
  NAccount = Account#account{strength = Strength},
  {success,NAccount,["strength"]};

execute(close_new_guide, Uin,_Data,Account)when is_integer(Uin) ->                   %关闭新手指导
  ?FILE_LOG_DEBUG("cache_admin_work_proc=>close_newer_guide", []),
  Addition = Account#account.addition#addition{newer_guide_steps = 1000},
  NAccount = Account#account{addition = Addition},
  {success,NAccount,["addition"]};

execute(modify_tower_equipment, Uin, {EquipID, EquipNo, EquipLevel}, Account) when is_integer(Uin) andalso is_record(Account, account) andalso is_list(EquipID) andalso is_list(EquipNo) andalso is_integer(EquipLevel)->
  CharacterList = Account#account.heros#heros.character_lists,
  MaxLevel = cache_api:get_equipment_max_level(Account),
  NLevel =
    if
      EquipLevel > MaxLevel -> MaxLevel;
      EquipLevel =< 0 -> 1;
      true -> EquipLevel
    end,

  Exp = cache_api:get_equipment_total_exp(NLevel - 1),

  {ETower, EquipItem} =
    lists:foldr(
      fun(Tower, {TmpTower, TmpEquip}) ->
        case cache_util:find_equipment_by_id(Tower#character.equipment_list, EquipID) of
          {success, EquipmentItem} -> {Tower, EquipmentItem};
          _ -> {TmpTower, TmpEquip}
        end
      end, {undefined, undefined}, CharacterList),
  if
    ETower =/= undefined andalso EquipItem =/= undefined ->
      ok;
    true ->
      ?FILE_LOG_DEBUG("invalid equipment id, ~p", [EquipID]),
      throw({custom, "HintSystemError"})
  end,

  NEquip = EquipItem#equipment{exp = Exp},
  NList = cache_util:update_equipment(ETower#character.equipment_list, NEquip),
  NTower = ETower#character{equipment_list = NList},
  NCharacterList = cache_util:update_tower(CharacterList, NTower),
  NAccount = Account#account{heros = #heros{character_lists = NCharacterList}},
  {success, NAccount, ["hero"]};

execute(add_backpack_equipment, _Uin, {EquipmentList, Level}, Account) when is_list(EquipmentList) andalso Level > 0 andalso is_record(Account, account) ->
  {success, Account,[]};

execute(minus_backpack_equipment, _Uin, EquipmentIDList, Account) when is_list(EquipmentIDList) andalso is_record(Account, account) ->
  {success, Account, []};

execute(add_backpack_material, _Uin, MaterialList, Account) when is_list(MaterialList) andalso is_record(Account, account) ->
  {success, Account, []};

execute(add_backpack_inscription, Uin, InscriptionList, Account) when is_list(InscriptionList) andalso is_record(Account, account) ->
  ?FILE_LOG_DEBUG("add_backpack_inscription => uin = ~p, list = ~p", [Uin, InscriptionList]),
  NInscriptionList =
    lists:map(
      fun(ID) ->
        #res_inscription{type = Pos} = cache_csv:get_inscription_by_id(ID),
        NPos =
          if
            Pos > 0 -> Pos;
            true -> 0
          end,
        #inscription{id = ID, pos = NPos}
      end, InscriptionList),
  NInscription = Account#account.backpack#backpack.inscription_list ++ NInscriptionList,
  NBackpack = Account#account.backpack#backpack{inscription_list = NInscription},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(minus_backpack_inscription, Uin, InscriptionList, Account) when is_list(InscriptionList) andalso is_record(Account, account) ->
  ?FILE_LOG_DEBUG("minus_backpack_inscription => uid = ~p, list = ~p", [Uin, InscriptionList]),
  NInscriptionList =
    lists:map(
      fun(ID) ->
        #res_inscription{type = Pos} = cache_csv:get_inscription_by_id(ID),
        NPos =
          if
            Pos > 0 -> Pos;
            true -> 0
          end,
        #inscription{id = ID, pos = NPos}
      end, InscriptionList),
  NInscription = Account#account.backpack#backpack.inscription_list -- NInscriptionList,
  NBackpack = Account#account.backpack#backpack{inscription_list = NInscription},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(modify_backpack_inscription, Uin, {ID, NID}, Account) when is_record(Account, account) ->
  ?FILE_LOG_DEBUG("modify_backpack_inscription => uid = ~p, info = ~p", [Uin, {ID, NID}]),
  #res_inscription{type = Pos} = cache_csv:get_inscription_by_id(ID),
  #res_inscription{type = NPos} = cache_csv:get_inscription_by_id(NID),
  DInscription = Account#account.backpack#backpack.inscription_list -- [#inscription{pos = Pos, id = ID}],
  NInscription = DInscription ++ [#inscription{pos = NPos, id = NID}],
  NBackpack = Account#account.backpack#backpack{inscription_list = NInscription},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(add_backpack_inscription_piece, Uin, PieceListList, Account) when is_list(PieceListList) andalso is_record(Account, account) ->
  ?FILE_LOG_DEBUG("add_backpack_inscription_piece => uin = ~p, list = ~p", [Uin, PieceListList]),
  NAddList =
    lists:map(
      fun({ID, Count}) ->
        cache_csv:get_inscription_piece_config_by_id(ID),
        NCount =
          if
            Count > 0 -> Count;
            true -> 0
          end,
        {ID, NCount}
      end, PieceListList),
  NPieceTree = cache_api:inc_tree_element(NAddList, Account#account.backpack#backpack.inscription_piece_list),
  NBackpack = Account#account.backpack#backpack{inscription_piece_list = NPieceTree},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(minus_backpack_inscription_piece, Uin, {ID, Count}, Account) when is_record(Account, account) ->
  ?FILE_LOG_DEBUG("minus_backpack_inscription_piece => uid = ~p, inscription info = [~p, ~p]", [Uin, ID, Count]),
  cache_csv:get_inscription_piece_config_by_id(ID),
  NCount =
    if
      Count > 0 ->
        Count;
      true ->
        0
    end,
  NInscriptionTree = cache_api:dec_tree_element([{ID, NCount}], Account#account.backpack#backpack.inscription_piece_list),
  NBackpack = Account#account.backpack#backpack{inscription_piece_list = NInscriptionTree},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(add_backpack_equipment_piece, Uin, PieceListList, Account) when is_list(PieceListList) andalso is_record(Account, account) ->
  ?FILE_LOG_DEBUG("add_backpack_equipment_piece => uin = ~p, list = ~p", [Uin, PieceListList]),
  NAddList =
    lists:map(
      fun({ID, Count}) ->
        cache_csv:get_equip_piece_config_by_id(ID),
        NCount =
          if
            Count > 0 -> Count;
            true -> 0
          end,
        {ID, NCount}
      end, PieceListList),
  NPieceTree = cache_api:inc_tree_element(NAddList, Account#account.backpack#backpack.equip_piece),
  NBackpack = Account#account.backpack#backpack{equip_piece = NPieceTree},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(add_backpack_property, Uin, PropertyListList, Account) when is_list(PropertyListList) andalso is_record(Account, account) ->
  ?FILE_LOG_DEBUG("add_backpack_property => uin = ~p, list = ~p", [Uin, PropertyListList]),
  NAddList =
    lists:map(
      fun({ID, Count}) ->
        cache_csv:get_property_config(ID),
        NCount =
          if
            Count > 0 -> Count;
            true -> 0
          end,
        {ID, NCount}
      end, PropertyListList),
  NPropertyTree = cache_api:inc_tree_element(NAddList, Account#account.backpack#backpack.prop_list),
  NBackpack = Account#account.backpack#backpack{prop_list = NPropertyTree},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(add_backpack_capacity, Uin, CapacityVal, Account) when is_integer(CapacityVal) andalso is_record(Account, account) ->
  ?FILE_LOG_DEBUG("add_backpack_capacity => uin = ~p, val = ~p", [Uin, CapacityVal]),
  NVal =
    if
      CapacityVal > 0 -> CapacityVal;
      true -> 0
    end,
  SumCapacity =  Account#account.backpack#backpack.capacity + NVal,
  NBackpack = Account#account.backpack#backpack{capacity = SumCapacity},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(minus_backpack_capacity, Uin, CapacityVal, Account) when is_integer(CapacityVal) andalso is_record(Account, account) ->
  ?FILE_LOG_DEBUG("minus_backpack_capacity => uin = ~p, val = ~p", [Uin, CapacityVal]),
  NVal =
    if
      CapacityVal > 0 -> CapacityVal;
      true -> 0
    end,
  SumCapacity =  Account#account.backpack#backpack.capacity - NVal,
  NCapacityVal =
    if
      SumCapacity < 0 -> 0;
      true -> SumCapacity
    end,
  NBackpack = Account#account.backpack#backpack{capacity = NCapacityVal},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(minus_backpack_material, _Uin, {_ID, _Count}, Account) when is_record(Account, account) ->
  {success, Account, []};

execute(minus_backpack_property, Uin, {ID, Count}, Account) when is_record(Account, account) ->
  ?FILE_LOG_DEBUG("minus_backpack_property => uin = ~p, property info = [~p, ~p]", [Uin, ID, Count]),
  cache_csv:get_property_config(ID),
  NCount =
    if
      Count > 0 -> Count;
      true  -> 0
    end,
  NPropertyTree = cache_api:dec_tree_element([{ID, NCount}], Account#account.backpack#backpack.prop_list),
  NBackpack = Account#account.backpack#backpack{prop_list = NPropertyTree},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(clear_backpack, Uin, _, Account) when is_record(Account, account) ->
  ?FILE_LOG_DEBUG("clear_backpack => uin = ~p", [Uin]),
  NBackpack = Account#account.backpack#backpack{equipment_list = [], material_list = gb_trees:empty(), prop_list = gb_trees:empty(), equip_piece = gb_trees:empty(), inscription_piece_list = gb_trees:empty(), inscription_list = []},
  NAccount = Account#account{backpack = NBackpack},
  {success, NAccount, ["backpack"]};

execute(set_tollgate_progress, Uin, {TollgateID, TollgateStar, TollgateScore}, Account) when is_record(Account, account) ->
  ?FILE_LOG_DEBUG("set_tollgate_progress => uin = ~p, tollgate ID = ~p, tollgate star = ~p, score = ~p", [Uin, TollgateID, TollgateStar, TollgateScore]),
  if
    TollgateID < 0 -> throw({custom, "TollgateIDError"});
    TollgateID > 90 -> throw({custom, "TollgateIDError"});
    true -> ok
  end,
  {NStar, StarSeq} =
    case TollgateStar of
      1 -> {1, [0, -1, -1]};
      2 -> {2, [0, 0, -1]};
      _ -> {3, [0, 0, 0]}
    end,
  NScore =
    if
      TollgateScore > 5000 -> 5000;
      TollgateScore < 0 -> 1;
      true -> TollgateScore
    end,

  BaseTollgateList =
    lists:map(
      fun(Index) ->
        TollgateConfig = cache_csv:get_tollgate_config(Index),
        #tollgate{
          id = Index,
          max_star = NStar,
          max_score = NScore,
          daily_remain_times = TollgateConfig#res_stage.daily_restrict_count,
          addition_remain_times = TollgateConfig#res_stage.daily_addition_count,
          last_pass_ts = 0,
          cool_time = TollgateConfig#res_stage.cool_time,
          max_star_seq = StarSeq
        }
      end, lists:seq(1, TollgateID)),
  NStage = Account#account.stage#stage{base_tollgate_list = BaseTollgateList},
  NAccount = Account#account{stage = NStage},
  {success, NAccount, ["stage"]};

execute(reset_login_reward, Uin, _, Account) when is_record(Account, account) ->
  ?FILE_LOG_DEBUG("reset_login_rewar => uin = ~p", [Uin]),
  NLogin = Account#account.login_reward#login_reward{login_reward_list = [], login_times = 1, latest_login_ts = dd_util:timestamp()},
  NAccount = Account#account{login_reward = NLogin},
  {success, NAccount, ["login_reward"]};

execute(reset_buy_energy, Uin, _, Account) when is_record(Account, account) ->
  ?FILE_LOG_DEBUG("reset_buy_energy => uin = ~p", [Uin]),
  NStrength = Account#account.strength#strength{today_buy_times = 0},
  NAccount = Account#account{strength = NStrength},
  {success, NAccount, ["strength"]};

execute(set_newer_guide, Uin, Operation, Account) when is_record(Account, account) andalso is_integer(Operation) ->
  ?FILE_LOG_DEBUG("set_newer_guide => uin = ~p, operation = ~p", [Uin, Operation]),
  NAddition =
    case Operation of
      0 ->
        Account#account.addition#addition{newer_guide_steps = 1000};
      1 ->
        Account#account.addition#addition{newer_guide_steps = 0};
      Type ->
        ?FILE_LOG_DEBUG("set newer_guide =>  error type = ~p", [Type]),
        throw({custom, "ErrorOperationType"})
    end,
  NAccount = Account#account{addition = NAddition},
  {success, NAccount, ["addition"]};

execute(delete_player, Uin, _Operation, _Account)when is_integer(Uin) ->                   %踢出玩家
  ?FILE_LOG_DEBUG("delete_player=> uin = ~p", [Uin]),
  {success, SessionNode} = dd_ms:read_config(session_node),
  case cache_work_proc:del_account(Uin) of
    success ->
      success = rpc:call(SessionNode, session_work, delete_session_by_uin, [Uin]),
      success;
    _ -> {fail,"DeleteError"}
  end;

execute(clear_player_data, Uin, _Operation, _Account)when is_integer(Uin) ->                   %踢出玩家
  ?FILE_LOG_DEBUG("clear_player_data=> uin = ~p", [Uin]),
  {success, SessionNode} = dd_ms:read_config(session_node),
  case cache_work_proc:del_account(Uin) of
    success ->
      success = rpc:call(SessionNode, session_work, delete_session_by_uin, [Uin]),
      success;
    _ -> {fail,"ClearPlayerDataError"}
  end.

get_backpack(Account) when is_record(Account, account) ->
  MaxLevel = cache_api:get_equipment_max_level(Account),
  EquipList =
    lists:map(
      fun(Equipment) ->
        EquipConfig = cache_csv:get_equipment_config(Equipment#equipment.no),
        Atk = cache_api:get_equipment_atk(Equipment#equipment.no, Equipment#equipment.exp, MaxLevel),
        #admin_equip_item{
          equip_id = Equipment#equipment.id,
          equip_no = Equipment#equipment.no,
          equip_name = EquipConfig#res_equipment.name,
          equip_atk = Atk,
          equip_level = cache_api:get_equipment_level(Equipment#equipment.exp, MaxLevel),
          equip_star = EquipConfig#res_equipment.star_level,
          equip_type = EquipConfig#res_equipment.type
        }
      end, Account#account.backpack#backpack.equipment_list),
  MaterialList =
    lists:map(
      fun({ID, Count}) ->
        MaterialConfig = cache_csv:get_material_config(ID),
        #admin_material_item{
          material_id = ID,
          material_name = MaterialConfig#res_material.name,
          material_count = Count
        }
      end, gb_trees:to_list(Account#account.backpack#backpack.material_list)),
  PropertyList =
    lists:map(
      fun({ID, Count}) ->
        PropertyConfig = cache_csv:get_property_config(ID),
        #admin_property_item{
          prop_id = ID,
          prop_count = Count,
          prop_name = PropertyConfig#res_property.name
        }
      end, gb_trees:to_list(Account#account.backpack#backpack.prop_list)),
  InscriptionList =
    lists:map(
      fun(#inscription{id = ID}) ->
        InscriptionConfig = cache_csv:get_inscription_by_id(ID),
        #admin_inscription_item{
          inscription_id = ID,
          inscription_star = InscriptionConfig#res_inscription.star,
          inscription_type = InscriptionConfig#res_inscription.type
        }
      end, Account#account.backpack#backpack.inscription_list),
  InscriptionPieceList =
    lists:map(
      fun({ID, Count}) ->
        InscriptionPieceConfig = cache_csv:get_inscription_piece_config_by_id(ID),
        #admin_inscription_piece_item{
          inscription_piece_id = ID,
          inscription_piece_name = InscriptionPieceConfig#res_inscription_piece.name,
          inscription_piece_star = InscriptionPieceConfig#res_inscription_piece.star,
          inscription_piece_count = Count
        }
      end, gb_trees:to_list(Account#account.backpack#backpack.inscription_piece_list)),
  %superstart属性
  S_equipment_list =
    lists:map(
	  fun({ID, {UUID,Count}}) ->
        #admin_superstar_item{
		  uuid = UUID,				  
          id = ID,
		  count = Count     
        }
      end, gb_trees:to_list(Account#account.backpack#backpack.s_equipment_list)),
  S_material_list =
    lists:map(
      fun({ID, {UUID,Count}}) ->
        #admin_superstar_item{
		  uuid = UUID,				  
          id = ID,
		  count = Count     
        }
      end, gb_trees:to_list(Account#account.backpack#backpack.s_material_list)),
  S_fragment_list =
    lists:map(
      fun({ID, {UUID,Count}}) ->
        #admin_superstar_item{
		  uuid = UUID,				  
          id = ID,
		  count = Count     
        }
      end, gb_trees:to_list(Account#account.backpack#backpack.s_fragment_list)),
  S_consumables_list =
    lists:map(
      fun({ID, {UUID,Count}}) ->
        #admin_superstar_item{
		  uuid = UUID,				  
          id = ID,
		  count = Count     
        }
      end, gb_trees:to_list(Account#account.backpack#backpack.s_consumables_list)),
  S_card_list =
    lists:map(
      fun({ID, {UUID,Count}}) ->
        #admin_superstar_item{
		  uuid = UUID,				  
          id = ID,
		  count = Count     
        }
      end, gb_trees:to_list(Account#account.backpack#backpack.s_card_list)),
  #admin_backpack{
    equip_list = EquipList,
    material_list = MaterialList,
    property_list = PropertyList,
    inscription_list = InscriptionList,
    inscription_piece_list = InscriptionPieceList,
    capacity = Account#account.backpack#backpack.capacity,
	%superstar属性
	s_equipment_list=S_equipment_list,
	s_material_list=S_material_list,
	s_fragment_list=S_fragment_list,
	s_consumables_list=S_consumables_list,
	s_card_list=S_card_list
  }.


get_json_value(Key, List) ->
  case proplists:get_value(Key, List, undefined) of
    undefined ->
      throw({custom, "HintSystemDataError"});
    Value -> Value
  end.

get_user_atk(Account) when is_record(Account, account) ->
  MaxLevel = cache_api:get_equipment_max_level(Account),
  lists:foldr(
    fun(Tower, TmpSum) ->
      TmpSum +
        lists:foldr(
          fun(Equipment, TmpEquipAtk) ->
            TmpEquipAtk + cache_api:get_equipment_atk(Equipment#equipment.no, Equipment#equipment.exp, MaxLevel)
          end, 0, Tower#character.equipment_list)
    end, 0, Account#account.heros#heros.character_lists).

get_user_tower_info(Account) when is_record(Account, account) ->
  MaxLevel = cache_api:get_equipment_max_level(Account),
  List =
    lists:map(
      fun(Tower) ->
        TowerConfig = cache_csv:get_tower_config(Tower#character.id),
        {EquipList,TotalAtk} =
          lists:foldr(
            fun(Equipment, {TmpList, TmpAtk}) ->
              EquipConfig = cache_csv:get_equipment_config(Equipment#equipment.no),
              Atk = cache_api:get_equipment_atk(Equipment#equipment.no, Equipment#equipment.exp, MaxLevel),
              EquipItem = #admin_equip_item{
                equip_id = Equipment#equipment.id,
                equip_no = Equipment#equipment.no,
                equip_name = EquipConfig#res_equipment.name,
                equip_atk = Atk,
                equip_level = cache_api:get_equipment_level(Equipment#equipment.exp, MaxLevel),
                equip_star = EquipConfig#res_equipment.star_level,
                equip_type = EquipConfig#res_equipment.type
              },
              {[EquipItem | TmpList], TmpAtk + Atk}
            end, {[], 0}, Tower#character.equipment_list),
        InscriptionList =
          lists:map(
            fun(#inscription{id = ID}) ->
              InscriptionConfig = cache_csv:get_inscription_by_id(ID),
              #admin_inscription_item{
                inscription_id = ID,
                inscription_star = InscriptionConfig#res_inscription.star,
                inscription_type = InscriptionConfig#res_inscription.type
              }
            end, Tower#character.inscription_list),
        #admin_tower_item{
          tower_no = Tower#character.id,
          tower_atk = TotalAtk,
          tower_equip_list = EquipList,
          tower_inscription_list = InscriptionList,
          tower_name = TowerConfig#res_tower.name
        }
      end, Account#account.heros#heros.character_lists),
  #admin_tower_info{tower_list = List}.
