%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2014 19:03
%%%-------------------------------------------------------------------
-module(adminserver_util).
-author("Administrator").

-include("../../deps/file_log/include/file_log.hrl").
-include("adminserver.hrl").
-include("../dd_ms.hrl").
-include("../csv.hrl").

%% API
-export([get_cache_node/1,get_json_value/2]).

-export([
  undefined_check/2,
  false_check/2,
  empty_check/2
]).

-export([
  encode_json_tower/1,
  encode_json_backpack/1,
  get_online_player_count/0,
  get_database_node/0,
  encode_shop_item/1
]).

undefined_check(Value, Reason) ->
  param_check(Value, undefined, Reason).

false_check(Value, Reason) ->
  param_check(Value, false, Reason).

empty_check(Value, Reason) ->
  param_check(Value, [], Reason).


param_check(Value, Value, Reason) ->
  throw({custom, Reason});
param_check(Value, _, _) -> Value.

get_cache_node(Uin) ->
  {success, CacheHashRule} = dd_ms:read_config(cache_hash_rule),
  case hash_service_util:find_key_store_node(dd_util:to_list(Uin), CacheHashRule) of
    fail ->
      ?FILE_LOG_ERROR("no available data node, ~p", [Uin]),
      throw({custom, "HintSystemError"});
    {success, Node} when is_atom(Node) -> {success, Node}
  end.

get_database_node() ->
  {success, DBNode} = dd_ms:read_config(database_node),
  DBNode.

get_json_value(Key, List) ->
  case proplists:get_value(Key, List, undefined) of
    undefined ->
      throw({custom, "get_json_value_error"});
    Value -> Value
  end.

encode_json_backpack(Backpack) when is_record(Backpack, admin_backpack) ->
	%superstar属性
	S_equipment_list =
	    lists:map(
	      fun(SEquipItem) ->
	        {
				struct,
				[
					{<<"id">>, dd_util:to_binary(SEquipItem#admin_superstar_item.id)},
	            	{<<"count">>, SEquipItem#admin_superstar_item.count}
				]   
	        }
	      end, Backpack#admin_backpack.s_equipment_list),
	S_material_list =
	    lists:map(
	      fun(SMaterialItem) ->
	        {
				struct,
				[
					{<<"id">>, dd_util:to_binary(SMaterialItem#admin_superstar_item.id)},
	            	{<<"count">>, SMaterialItem#admin_superstar_item.count}
				]   
	        }
	      end, Backpack#admin_backpack.s_material_list),
	S_fragment_list =
	    lists:map(
	      fun(SFragmentItem) ->
	        {
				struct,
				[
				 	{<<"uuid">>,dd_util:to_binary(SFragmentItem#admin_superstar_item.id)},
					{<<"id">>, SFragmentItem#admin_superstar_item.id},
	            	{<<"count">>, SFragmentItem#admin_superstar_item.count}
				]   
	        }
	      end, Backpack#admin_backpack.s_fragment_list),
	S_consumables_list =
	    lists:map(
	      fun(SConsumablesItem) ->
	        {
				struct,
				[
					{<<"id">>, dd_util:to_binary(SConsumablesItem#admin_superstar_item.id)},
	            	{<<"count">>, SConsumablesItem#admin_superstar_item.count}
				]   
	        }
	      end, Backpack#admin_backpack.s_consumables_list),
	S_card_list =
	    lists:map(
	      fun(SCardItem) ->
	        {
				struct,
				[
					{<<"id">>, dd_util:to_binary(SCardItem#admin_superstar_item.id)},
	            	{<<"count">>, SCardItem#admin_superstar_item.count}
				]   
	        }
	      end, Backpack#admin_backpack.s_equipment_list),
	{
	    struct,
	    [
		  	%superstart属性
			{<<"s_equipment">>, S_equipment_list},
			{<<"s_material">>, S_material_list},
			{<<"s_fragment">>, S_fragment_list},
			{<<"s_consumables">>, S_consumables_list},
			{<<"s_card">>, S_card_list}
	    ]
  	}.

encode_json_tower(TowerInfo) when is_record(TowerInfo, admin_tower_info) ->
  lists:map(
    fun(AdminTowerIntem) ->
      EqupList =
        lists:map(
          fun(EquipItem) ->
            {
              struct,
              [
                {<<"uid">>, dd_util:to_binary(EquipItem#admin_equip_item.equip_id)},
                {<<"id">>, dd_util:to_binary(EquipItem#admin_equip_item.equip_no)},
                {<<"type">>, EquipItem#admin_equip_item.equip_type},
                {<<"name">>, dd_util:to_binary(EquipItem#admin_equip_item.equip_name)},
                {<<"level">>, EquipItem#admin_equip_item.equip_level},
                {<<"star">>, EquipItem#admin_equip_item.equip_star},
                {<<"atk">>, EquipItem#admin_equip_item.equip_atk}
              ]
            }
          end, AdminTowerIntem#admin_tower_item.tower_equip_list),
      InscriptionList =
        lists:map(
          fun(InscriptionItem) ->
            {
              struct,
              [
                {<<"inscription_id">>, dd_util:to_binary(InscriptionItem#admin_inscription_item.inscription_id)},
                {<<"inscription_type">>, dd_util:to_binary(InscriptionItem#admin_inscription_item.inscription_type)},
                {<<"inscription_star">>, dd_util:to_binary(InscriptionItem#admin_inscription_item.inscription_star)}
              ]
            }
          end, AdminTowerIntem#admin_tower_item.tower_inscription_list),
      {
        struct,
        [
          {<<"id">>, dd_util:to_binary(AdminTowerIntem#admin_tower_item.tower_no)},
          {<<"name">>, dd_util:to_binary(AdminTowerIntem#admin_tower_item.tower_name)},
          {<<"atk">>, AdminTowerIntem#admin_tower_item.tower_atk},
          {<<"equip">>, EqupList},
          {<<"inscription">>, InscriptionList}
        ]
      }
    end, TowerInfo#admin_tower_info.tower_list).

get_online_player_count() ->
  {success, GateWayNodes} = dd_ms:get_all_gateway(),
  ?FILE_LOG_DEBUG("get_all_gateway ok ",[]),
  lists:foldr(
    fun(Node, TmpCount) ->
      case rpc:call(Node#gateway.node, gateway_monitor, get_online_user_count, []) of
        {success, Count} ->
          ?FILE_LOG_DEBUG("success,Count = ~p",[Count]),
          TmpCount + Count;
        Other ->
          ?FILE_LOG_ERROR("get online user count error, reason = ~p", [Other]),
          TmpCount
      end
    end, 0, GateWayNodes).

encode_shop_item(ShopItemList)when is_list(ShopItemList)->        %ShopItemList的每一项为一个 record
  lists:map(
    fun(Res_goods)when is_record(Res_goods,res_goods) ->
      {
        struct,
        [
          {<<"id">>,dd_util:to_binary(Res_goods#res_goods.id) },
          {<<"name">>,dd_util:to_binary(Res_goods#res_goods.name) },
          {<<"pic">>,dd_util:to_binary(Res_goods#res_goods.pic)},
          {<<"goods_no">>,dd_util:to_binary(Res_goods#res_goods.goods_no) },
          {<<"goods_type">>,Res_goods#res_goods.goods_type },
          {<<"goods_count">>,Res_goods#res_goods.goods_count},
          {<<"money_type">>,Res_goods#res_goods.money_type},
          {<<"money_count">>,dd_util:to_binary(Res_goods#res_goods.money_count)},
          {<<"gift_type">>, Res_goods#res_goods.gift_type},
          {<<"gift_count">>,Res_goods#res_goods.gift_count },
          {<<"restrict_count">>,Res_goods#res_goods.restrict_count },
          {<<"is_recommend">>,Res_goods#res_goods.is_recommend },
          {<<"start_ts">>,Res_goods#res_goods.start_ts},
          {<<"over_ts">>,Res_goods#res_goods.over_ts}
        ]
      }
    end, ShopItemList).
