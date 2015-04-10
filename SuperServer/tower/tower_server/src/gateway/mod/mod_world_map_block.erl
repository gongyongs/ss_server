%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 八月 2014 下午2:32
%%%-------------------------------------------------------------------
-module(mod_world_map_block).
-author("zqlt").

-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").

%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check(Req:get(method)=:= ?POST, "request method error"),
  PostData = Req:parse_post(),
  gateway_util:signature_check(PostData),

  SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  WBlockID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("world_block_id", PostData, undefined), "HintRequestDataError")),
  GetMaterials = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("get_material", PostData, undefined), "HintRequestDataError")),

  gateway_util:empty_check(WBlockID, "HintRequestDataError"),

  {struct, DataList} = mochijson2:decode(GetMaterials),
  {success, MaterialList} = gateway_util:decode_world_map_block_material(DataList),

  {success, Uin} = gateway_util:get_uin_by_session(SessionID, world_map_block),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  case rpc:call(CacheNode, cache, click_world_map_block, [Uin, WBlockID, MaterialList]) of
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