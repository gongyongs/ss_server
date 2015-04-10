%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 八月 2014 上午10:10
%%%-------------------------------------------------------------------
-module(mod_strengthen).
-author("zqlt").

-include("../gateway.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../cache/cache_def.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check(Req:get(method) =:= ?POST, "request method error"),
  PostData = Req:parse_post(),
  gateway_util:signature_check(PostData),

  SessionId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  Type = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("type", PostData, undefined), "HintRequestDataError")),
  ObjectType = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("object_type", PostData, undefined), "HintRequestDataError")),
  ObjectID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("object_id", PostData, undefined), "HintRequestDataError")),
  TowerID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("tower_id", PostData, undefined), "HintRequestDataError")),
  StrengthenMaterialData = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("material", PostData, undefined), "HintRequestDataError")),

  ?FILE_LOG_DEBUG("mod_strength => [session = ~p, type = ~p,object_type  = ~p,object_id =  ~p,tower_id = ~p,material = ~p]", [SessionId, Type, ObjectType, ObjectID, TowerID, StrengthenMaterialData]),

  TargetID =
    case Type of
      0 -> %%升级
        [];
      1 -> %%进阶
        dd_util:to_list(gateway_util:undefined_check(proplists:get_value("target_id", PostData, undefined), "HintRequestDataError"))
    end,

  {struct, DataList} = mochijson2:decode(StrengthenMaterialData),
  {_EquipList, MaterialList} = gateway_util:decode_strengthen_material(DataList),

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, strengthen),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),
  ?FILE_LOG_DEBUG("mod_strength => [uin = ~p, type = ~p,object_type  = ~p,object_id =  ~p,tower_id = ~p,material = ~p]", [Uin, Type, ObjectType, ObjectID, TowerID, StrengthenMaterialData]),
  case rpc:call(CacheNode, cache, strengthen, [Uin, {Type, ObjectType, ObjectID, TargetID, TowerID, MaterialList}])  of
    {success, DataBin} ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, DataBin},
          {<<"sign">>, gateway_util:back_signature(DataBin)}
        ]
      };
    {fail, Reason} ->
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.

