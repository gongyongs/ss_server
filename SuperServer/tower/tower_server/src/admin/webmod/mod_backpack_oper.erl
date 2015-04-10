%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十一月 2014 上午9:56
%%%-------------------------------------------------------------------
-module(mod_backpack_oper).
-export([req_handle/1]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  PostData = Req:parse_post(),

  Uin = dd_util:to_integer(proplists:get_value("uin", PostData, undefined)),
  Action = list_to_atom(dd_util:to_list(proplists:get_value("action", PostData, undefined))),
  Data = dd_util:to_list(proplists:get_value("data", PostData, undefined)),
  DecodeData = decode_data(Action, Data),
  {success,Node} = adminserver_util:get_cache_node(Uin),

  ?FILE_LOG_DEBUG("backpack_oper => uin = ~p, action = ~p, data = ~p", [Uin, Action, DecodeData]),

  case rpc:call(Node, cache_admin, backpack_oper,[Uin, Action, DecodeData]) of
    success ->
      {
        struct,
        [
          {<<"result">>, 0}
        ]
      };
    Other ->
      ?FILE_LOG_DEBUG("backpack_oper => action = ~p, param = ~p, reason = ~p", [Action, DecodeData, Other]),
      {
        struct,
        [
          {<<"result">>, -1}
        ]
      }
  end.

%%gm增加球员碎片
decode_data(add_backpack_player_piece,Data)->
	{struct,JsonList} = mochijson2:decode(Data),
	PlayerPieceList = dd_util:to_list(adminserver_util:get_json_value(<<"key">>,JsonList)),
	Count = dd_util:to_integer(adminserver_util:get_json_value(<<"val">>,JsonList)),
	lists:map(
	  fun(Id)->
			{Id,Count}
	  end, PlayerPieceList);

decode_data(add_backpack_equipment, Data) ->
  {struct, JsonList} = mochijson2:decode(Data),
  EquipmentJson = dd_util:to_list(adminserver_util:get_json_value(<<"equipment">>, JsonList)),
  Level = dd_util:to_integer(adminserver_util:get_json_value(<<"level">>, JsonList)),
  {string:tokens(EquipmentJson, ","), Level};

decode_data(minus_backpack_equipment, Data) ->
  string:tokens(Data, ",");

decode_data(add_backpack_material, Data) ->
  {struct, DataList} = mochijson2:decode(Data),
  MaterialIDList = dd_util:to_list(adminserver_util:get_json_value(<<"key">>, DataList)),
  Count = dd_util:to_integer(adminserver_util:get_json_value(<<"val">>, DataList)),
  List = string:tokens(MaterialIDList, ","),
  lists:map(
    fun(ID) ->
      {ID, Count}
    end, List);

decode_data(add_backpack_property, Data) ->
  {struct, DataList} = mochijson2:decode(Data),
  PropertyIDList = dd_util:to_list(adminserver_util:get_json_value(<<"key">>, DataList)),
  Count = dd_util:to_integer(adminserver_util:get_json_value(<<"val">>, DataList)),
  List = string:tokens(PropertyIDList, ","),
  lists:map(
    fun(ID) ->
      {ID, Count}
    end, List);

decode_data(add_backpack_inscription, Data) ->
  {struct, JsonList} = mochijson2:decode(Data),
  InscriptionList = dd_util:to_list(adminserver_util:get_json_value(<<"inscription">>, JsonList)),
  string:tokens(InscriptionList, ",");

decode_data(minus_backpack_inscription, Data) ->
  {struct, JsonList} = mochijson2:decode(Data),
  InscriptionList = dd_util:to_list(adminserver_util:get_json_value(<<"inscription">>, JsonList)),
  string:tokens(InscriptionList, ",");

decode_data(modify_backpack_inscription, Data) ->
  {struct, JsonList} = mochijson2:decode(Data),
  ID = dd_util:to_list(adminserver_util:get_json_value(<<"id">>, JsonList)),
  NID = dd_util:to_list(adminserver_util:get_json_value(<<"nid">>, JsonList)),
  {ID, NID};

decode_data(add_backpack_inscription_piece, Data) ->
  {struct, DataList} = mochijson2:decode(Data),
  PropertyIDList = dd_util:to_list(adminserver_util:get_json_value(<<"key">>, DataList)),
  Count = dd_util:to_integer(adminserver_util:get_json_value(<<"val">>, DataList)),
  List = string:tokens(PropertyIDList, ","),
  lists:map(
    fun(ID) ->
      {ID, Count}
    end, List);

decode_data(minus_backpack_inscription_piece, Data) ->
  {struct, DataList} = mochijson2:decode(Data),
  ID = dd_util:to_list(adminserver_util:get_json_value(<<"key">>, DataList)),
  Count = dd_util:to_integer(adminserver_util:get_json_value(<<"val">>, DataList)),
  {ID, Count};

decode_data(add_backpack_equipment_piece, Data) ->
  {struct, DataList} = mochijson2:decode(Data),
  PropertyIDList = dd_util:to_list(adminserver_util:get_json_value(<<"key">>, DataList)),
  Count = dd_util:to_integer(adminserver_util:get_json_value(<<"val">>, DataList)),
  List = string:tokens(PropertyIDList, ","),
  lists:map(
    fun(ID) ->
      {ID, Count}
    end, List);

decode_data(add_backpack_capacity, Data) ->
  dd_util:to_integer(Data);

decode_data(minus_backpack_capacity, Data) ->
  dd_util:to_integer(Data);

decode_data(minus_backpack_material, Data) ->
  {struct, DataList} = mochijson2:decode(Data),
  MaterialID = dd_util:to_list(adminserver_util:get_json_value(<<"key">>, DataList)),
  Count = dd_util:to_integer(adminserver_util:get_json_value(<<"value">>, DataList)),
  {MaterialID, Count};

decode_data(minus_backpack_property, Data) ->
  {struct, DataList} = mochijson2:decode(Data),
  ID = dd_util:to_list(adminserver_util:get_json_value(<<"key">>, DataList)),
  Count = dd_util:to_integer(adminserver_util:get_json_value(<<"value">>, DataList)),
  {ID, Count};

decode_data(clear_backpack, Data) -> Data;

decode_data(_, _) ->
  throw({custom, "unknow type"}).




