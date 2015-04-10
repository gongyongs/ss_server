%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 十月 2014 下午12:42
%%%-------------------------------------------------------------------
-module(mod_admin_action).
-author(j).
-export([req_handle/1]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../cache/cache_def.hrl").
%% API

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),
  Uin = dd_util:to_integer(proplists:get_value("uin", GetData, undefined)),
  Data = dd_util:to_list(proplists:get_value("data", GetData, undefined)),
  Action = list_to_atom(dd_util:to_list(proplists:get_value("action", GetData, undefined))),

  ?FILE_LOG_DEBUG("handle_action => uin = ~p, action = ~p, data = ~p", [Uin, Action, Data]),

  DecodeData = decode_data(Action, Data),
  {success, CacheNode} = adminserver_util:get_cache_node(Uin),

  case rpc:call(CacheNode, cache_admin, handler_action, [Action, Uin, DecodeData]) of
    success ->
      {struct, [{<<"result">>, 0}]};
    Other ->
      ?FILE_LOG_DEBUG("handle_action => uin = ~p, action = ~p, fail reason = ~p", [Uin, Action, Other]),
      {struct, [{<<"result">>, -1}]}
  end.

decode_data(set_tollgate_progress, Data) ->
  {struct, DataList} = mochijson2:decode(Data),
  TollgateID = dd_util:to_integer(adminserver_util:get_json_value(<<"id">>, DataList)),
  TollgateStar = dd_util:to_integer(adminserver_util:get_json_value(<<"star">>, DataList)),
  TollgateScore = dd_util:to_integer(adminserver_util:get_json_value(<<"score">>, DataList)),
  {TollgateID, TollgateStar, TollgateScore};
decode_data(reset_login_reward, Data) -> Data;
decode_data(reset_buy_energy, Data) -> Data;
decode_data(set_newer_guide, Data) ->
  {struct, DataList} = mochijson2:decode(Data),
  dd_util:to_integer(adminserver_util:get_json_value(<<"operation">>, DataList));
decode_data(modify_tower_equipment, Data) ->
  {struct, DataList} = mochijson2:decode(Data),
  EquipID = dd_util:to_list(adminserver_util:get_json_value(<<"equip_id">>, DataList)),
  EquipNo = dd_util:to_list(adminserver_util:get_json_value(<<"equip_no">>, DataList)),
  EquipLevel = dd_util:to_integer(adminserver_util:get_json_value(<<"equip_level">>, DataList)),
  {EquipID, EquipNo, EquipLevel};
decode_data(_, _) -> throw({custom, "ErrorActionType"}).


