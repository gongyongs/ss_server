%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 八月 2014 下午2:41
%%%-------------------------------------------------------------------
-module(cache_api).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
-include("cache_def.hrl").
-include("../csv.hrl").
%% API
-export([
  get_equipment_level/2,
  get_equipment_total_exp/1,
  check_exp/2,
  get_gain_exp_by_level/2,
  get_total_exp_by_level/1,
  get_equipment_atk/3,
  get_user_atk/2,
  get_equipment_max_level/1,
  get_equipment_max_level_by_star_count/1
]).

-export([
  account_inc_money/4,
  account_dec_money/4
]).

-export([
  inc_strength/3,
  dec_strength/3,
  inc_tree_element/2,
  dec_tree_element/2
]).

-export([
  flatten_statistics_list/1,
  lists_to_str_with_split/2
]).

-export([
  inc_ts_count/3,
  dec_ts_count/3,
  check_count/2,
  check_status/2,
  insert_length_list/3
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flatten_statistics_list(List) when is_list(List) ->
  lists:foldr(
    fun({ID, Count}, TmpList)  ->
      NL = lists:map(fun(_Index) -> ID end, lists:seq(1, Count)),
      lists:merge(NL, TmpList)
    end, [], List).

lists_to_str_with_split(List, Split) when is_list(List) ->
  lists:foldr(
    fun(ID, TmpL) ->
      TmpL ++ dd_util:to_list(ID) ++ dd_util:to_list(Split)
    end, "", List).

%%增加体力
inc_strength(Account, Value, _Reason) when Value > 0 ->
  Strength = Account#account.strength#strength{strength = Account#account.strength#strength.strength + Value},
  Account#account{strength = Strength};
inc_strength(Account, _, _Reason) -> Account.

dec_strength(Account, 0, _Reason) -> Account;
dec_strength(Account, Value, _Reason) ->
  BeforeValue = Account#account.strength#strength.strength,
  if
    BeforeValue < Value -> throw({custom, "HintInsufficientEnergy"});
    true ->
      Strength = Account#account.strength#strength{strength = Account#account.strength#strength.strength - Value},
      Account#account{strength = Strength}
  end.

%%时间回复单位变化（如赏金赛入场券等）
inc_ts_count(Ts_info, Value, _Reason) when Value > 0  andalso is_record(Ts_info, ts_item) ->
  Ts_info#ts_item{count = Ts_info#ts_item.count + Value};
inc_ts_count(Ts_info, _, _Reason) -> Ts_info.

dec_ts_count(Ts_info, 0, _Reason) -> Ts_info;
dec_ts_count(Ts_info, Value, _Reason) when is_record(Ts_info, ts_item)->
  BeforeValue = Ts_info#ts_item.count,
  if
    BeforeValue < Value -> throw({custom, "HintInsufficientEnergy"});
    true -> Ts_info#ts_item{count = Ts_info#ts_item.count - Value}
  end.

%数量检测
check_count(Cost, Count) when is_integer(Cost) andalso is_integer(Count) ->
  if
    Cost > Count ->
      ?FILE_LOG_ERROR("have not sufficient count, [~p]", []),
      throw({custom, "HintInsufficientEnergy"});
    true -> ok
  end.

%状态检测
check_status(CurStatus,CheckStatus) ->
  if
    CurStatus /= CheckStatus ->
      ?FILE_LOG_ERROR("erro status, cur [~p] check[~p]", [CurStatus,CheckStatus]),
      throw({custom, "HintInsufficientEnergy"});
    true -> ok
  end.

inc_tree_element([], GBTree) -> GBTree;
inc_tree_element([{ID, Count} | T], Tree) when Count > 0 ->
  case gb_trees:lookup(ID, Tree) of
    none ->
      inc_tree_element(T, gb_trees:insert(ID, Count, Tree));
    {value, Num} ->
      inc_tree_element(T, gb_trees:update(ID, Count + Num, Tree))
  end;
inc_tree_element([{UUid,ID, Count} | T], Tree) when Count > 0 ->
  case gb_trees:lookup(ID, Tree) of
    none ->
      inc_tree_element(T, gb_trees:insert(ID, {UUid,Count}, Tree));
    {value, {_,Num}} ->
      inc_tree_element(T, gb_trees:update(ID, {UUid,Count + Num}, Tree))
  end.

dec_tree_element([], GBTree) -> GBTree;
dec_tree_element([{ID, Count} | T], Tree) ->
  case gb_trees:lookup(ID, Tree) of
    none -> throw({custom, "HintInsufficientMaterial"});
    {value,Num} ->
      if
        Num < Count -> throw({custom, "HintInsufficientMaterial"});
        Num =:= Count -> dec_tree_element(T, gb_trees:delete(ID, Tree));
        true -> dec_tree_element(T, gb_trees:update(ID, Num - Count, Tree))
      end
  end;
dec_tree_element([{UUid,ID, Count} | T], Tree) ->
  case gb_trees:lookup(ID, Tree) of
    none -> throw({custom, "HintInsufficientMaterial"});
    {value, {_,Num}} ->
      if
        Num < Count -> throw({custom, "HintInsufficientMaterial"});
        Num =:= Count -> dec_tree_element(T, gb_trees:delete(ID, Tree));
        true -> dec_tree_element(T, gb_trees:update(ID, {UUid,Num - Count}, Tree))
      end
  end.

insert_length_list(Element,[],Length) when Length > 0 -> [Element];
insert_length_list(Element,[H|T],Length) when Length > 0 ->
	if 
	   length([H|T]) < Length -> lists:append([H|T],[Element]);
	   length([H|T]) =:= Length -> lists:append(T,[Element])
	end.

%%添加装备
%% add_equipment([], _Uin, EquipmentList) ->  EquipmentList;
%% add_equipment([{EquipmentNo, Level} | Tail], Uin, EquipmentList) ->
%%   Exp = get_total_exp_by_level(Level - 1),
%%   {success, Uid} = cache_guid:alloc_guid(Uin),
%%   add_equipment(Tail, Uin, [#equipment{id = Uid, no = EquipmentNo, exp = Exp} | EquipmentList]).

%% add inscription
%% add_inscription([], _Uin, InscriptionList) ->
%%   InscriptionList;
%% add_inscription([{InscriptionId, Pos} | Tail], Uin, InscriptionList) ->
%%   {success, Uid} = cache_guid:alloc_guid(Uin),
%%   add_inscription(Tail, Uin, [#inscription{id = Uid, pos = Pos} | InscriptionList]).

%%添加材料
%% inc_material([], MaterialList) -> MaterialList;
%% inc_material([{MaterialID, Count} | Tail], MaterialList) ->
%%   case cache_util:find_material_by_id(MaterialList, MaterialID) of
%%     fail -> inc_material(Tail, [#material{id = MaterialID, count = Count} | MaterialList]);
%%     {success, Material} ->
%%       NCount = Count + Material#material.count,
%%       NMaterialList = cache_util:update_material(MaterialList, Material#material{count = NCount}),
%%       inc_material(Tail, NMaterialList)
%%   end.

%% inc_piece([], PieceList) -> PieceList;
%% inc_piece([{PieceID, Count} | Tail], PieceList) ->
%%   case cache_util:find_piece_by_id(PieceList, PieceID) of
%%     fail -> inc_piece(Tail, [#piece{id = PieceID, count = Count} | PieceList]);
%%     {success, Piece} ->
%%       NCount = Count + Piece#piece.count,
%%       NPieceList = cache_util:update_piece(PieceList, Piece#piece{count = NCount}),
%%       inc_material(Tail, NPieceList)
%%   end.
%%
%% dec_piece([], PieceList) -> PieceList;
%% dec_piece([{PieceID, Count} | Tail], PieceList) ->
%%   case cache_util:find_piece_by_id(PieceList, PieceID) of
%%     fail -> throw({custom, "HintInsufficientMaterial"});
%%     {success, Piece} ->
%%       NCount = Piece#piece.count - Count,
%%       if
%%         NCount < 0 -> throw({custom, "HintInsufficientMaterial"});
%%         NCount =:= 0 ->
%%           NPieceList = cache_util:delete_piece_by_id(PieceList, PieceID),
%%           dec_piece(Tail, NPieceList);
%%         true ->
%%           NPieceList = cache_util:update_piece(PieceList, Piece#piece{count = NCount}),
%%           dec_piece(Tail, NPieceList)
%%       end
%%   end.


%% dec_material([], MaterialList) -> MaterialList;
%% dec_material([{MaterialID, Count} | Tail], MaterialList) ->
%%   case cache_util:find_material_by_id(MaterialList, MaterialID) of
%%     fail -> throw({custom, "HintInsufficientMaterial"});
%%     {success, Material} ->
%%       NCount = Material#material.count - Count,
%%       if
%%         NCount < 0 -> throw({custom, "HintInsufficientMaterial"});
%%         NCount =:= 0 ->
%%           NMaterialList = cache_util:delete_material_by_id(MaterialList, MaterialID),
%%           dec_material(Tail, NMaterialList);
%%         true ->
%%           NMaterialList = cache_util:update_material(MaterialList, Material#material{count = NCount}),
%%           dec_material(Tail, NMaterialList)
%%       end
%%   end.


%%添加道具
%% inc_property([], PropertyList) -> PropertyList;
%% inc_property([{PropertyID, Count} | Tail], PropertyList) ->
%%   case cache_util:find_property_by_id(PropertyList, PropertyID) of
%%     fail -> inc_property(Tail, [#property{id = PropertyID, count = Count} | PropertyList]);
%%     {success, Property} ->
%%       NCount = Count + Property#property.count,
%%       NPropertyList = cache_util:update_property(PropertyList, Property#property{count = NCount}),
%%       inc_property(Tail, NPropertyList)
%%   end.
%%
%% dec_property([], PropertyList) -> PropertyList;
%% dec_property([{PropertyID, Count} | Tail], PropertyList) ->
%%   case cache_util:find_property_by_id(PropertyList, PropertyID) of
%%     fail -> throw({custom, "HintInsufficientTool"});
%%     {success, Property} ->
%%       NCount = Property#property.count - Count,
%%       if
%%         NCount < 0 -> throw({custom, "HintInsufficientTool"});
%%         true -> ok
%%       end,
%%       NPropertyList = cache_util:update_property(PropertyList, Property#property{count = NCount}),
%%       dec_property(Tail, NPropertyList)
%%   end.

account_inc_money(Account, 2, 0, _Reason) -> Account;
account_inc_money(Account, 2, Value, Reason) when Value > 0 ->
  ?FILE_LOG_INFO("player gold coin flow => add gold :uin = ~p, reason = ~p", [Account#account.uin, Reason]),
  cache_log:log_money_flow(Account#account.uin, Reason, 2, Account#account.gold_coin, Account#account.gold_coin + Value),
  Account#account{gold_coin = Account#account.gold_coin + Value};
account_inc_money(Account, 1, 0, _Reason) -> Account;
account_inc_money(Account, 1, Value, Reason) when Value > 0 ->
  ?FILE_LOG_INFO("player gem flow => add gem:uin = ~p, reason = ~p", [Account#account.uin, Reason]),
  cache_log:log_money_flow(Account#account.uin, Reason, 1, Account#account.gem, Account#account.gem + Value),
  Account#account{gem = Account#account.gem + Value}.

account_dec_money(Account, 2, 0, _Reason) -> Account;
account_dec_money(Account, 2, Value, Reason) when Value > 0 ->
  ?FILE_LOG_INFO("player gold coin flow => dec gold :uin = ~p, reason = ~p", [Account#account.uin, Reason]),
  cache_log:log_money_flow(Account#account.uin, Reason, 2, Account#account.gold_coin, Account#account.gold_coin - Value),
  Account#account{gold_coin = Account#account.gold_coin - Value};

account_dec_money(Account, 1, 0, _Reason) -> Account;
account_dec_money(Account, 1, Value, Reason) when Value > 0 ->
  ?FILE_LOG_INFO("player gem flow => dec gem :uin = ~p, reason = ~p", [Account#account.uin, Reason]),
  cache_log:log_money_flow(Account#account.uin, Reason, 1, Account#account.gem, Account#account.gem - Value),
  Account#account{gem = Account#account.gem - Value}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_equipment_max_level_by_star_count(TotalStar) when is_integer(TotalStar) ->
  Level = TotalStar,
  if
    Level =< 0 -> 1;
    Level > 270 -> 270;
    true -> Level
  end.
get_equipment_max_level(Account) when is_record(Account, account) ->
  TotalStar =
    lists:foldr(
      fun(Item, TmpSum) ->
        TmpSum + Item#tollgate.max_star
      end, 0, Account#account.stage#stage.base_tollgate_list),
  get_equipment_max_level_by_star_count(TotalStar).


%%根据当前经验值计算等级
get_equipment_level(CurExp, MaxLevel) when is_integer(CurExp) ->
  case check_exp(CurExp, MaxLevel) of
    success ->
      case get_equipment_level_1(lists:seq(1, MaxLevel), MaxLevel, CurExp) of
        {success, Level} -> Level;
        fail ->
          ?FILE_LOG_ERROR("get equipmemt level fail", []),
          throw({custom, "HintSystemDataError"})
      end;
    fail ->
      MaxLevel
  end.

get_equipment_total_exp(MaxLevel) ->
  get_total_exp_by_level(MaxLevel).

check_exp(Exp, MaxLevel) ->
  CurMaxLevelExp = get_total_exp_by_level(MaxLevel),
  if
    Exp =< CurMaxLevelExp  -> success;
    true -> fail
  end.

get_equipment_level_1([], _, _) -> fail;
get_equipment_level_1([H | T], MaxLevel,CurExp) ->
  Exp =  get_total_exp_by_level(H),
  if
    CurExp < Exp -> {success, H};
    CurExp =:= Exp andalso H < MaxLevel-> {success, H + 1};
    CurExp =:= Exp andalso H =:= MaxLevel -> {success, H};
    true -> get_equipment_level_1(T, MaxLevel, CurExp)
  end.


%%获取当前装备被吞噬后可获取的经验值
get_gain_exp_by_level(EquipmentNo, EquipmentLevel) ->
  EquipmentConfig = cache_csv:get_equipment_config(EquipmentNo),
  BaseGainExp = EquipmentConfig#res_equipment.gain_exp,
  BaseGainExpPerLevel = EquipmentConfig#res_equipment.gain_exp_per_level,
%%  MaxLevel = EquipmentConfig#res_equipment.max_level,
%%  PreMaxLevel = get_pre_max_level_by_cur_level(MaxLevel),
%%  LevelDif = EquipmentLevel - PreMaxLevel,
  BaseGainExp + BaseGainExpPerLevel * (EquipmentLevel - 1).

%% %%根据当前的最高等级获取前一阶的最高等级
%% get_pre_max_level_by_cur_level(CurMaxLevel) when is_integer(CurMaxLevel) ->
%%   Result =
%%     case CurMaxLevel of
%%       10 -> 0;
%%       25 -> 10;
%%       35 -> 25;
%%       60 -> 35;
%%       70 -> 60;
%%       75 -> 70;
%%       80 -> 75;
%%       85 -> 80;
%%       90 -> 85;
%%       100 -> 90
%%     end,
%%   dd_util:to_integer(Result).

get_total_exp_by_level(Level) when is_integer(Level) ->
  if
    Level > 0 ->
      ExpConfig = cache_csv:get_exp_config(Level),
      ExpConfig#res_exp_config.total_exp;
    true -> 0
  end.

get_equipment_atk(EquipmentNo, CurExp, MaxLevel) when is_list(EquipmentNo) andalso is_integer(CurExp) andalso is_integer(MaxLevel) ->
  case cache_csv:get_equipment_config_without_exception(EquipmentNo) of
    {success, EquipmentConfig} ->
      CurLevel = get_equipment_level(CurExp, MaxLevel),
      BaseAtk = EquipmentConfig#res_equipment.base_dsm,
      BaseAtkPerLevel = EquipmentConfig#res_equipment.dsm_per_level,
      BaseAtk + BaseAtkPerLevel * (CurLevel - 1);
    _ -> 0
  end.


get_user_atk(CharacterList, MaxLevel) when is_list(CharacterList) ->
  lists:foldr(
    fun(Tower, TmpSum) ->
      TmpSum +
        lists:foldr(
          fun(Equipment, TmpEquipAtk) ->
            TmpEquipAtk + cache_api:get_equipment_atk(Equipment#equipment.no, Equipment#equipment.exp, MaxLevel)
          end, 0, Tower#character.equipment_list)
    end, 0, CharacterList).




