%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 一月 2015 上午10:53
%%%-------------------------------------------------------------------
-module(mod_replace_inscription).
-author("zqlt").

-include("../../../deps/file_log/include/file_log.hrl").
-include("../../cache/cache_def.hrl").
-include("../gateway.hrl").
%% API
-export([req_handle/1]).

%% get_tower_by_id(TowerList, TowerID) ->
%%   F = fun(TowerItem, ID) -> TowerItem#character.id =:= ID end,
%%   cache_util:find_item_by_id(TowerList, F, TowerID).

req_handle(Req) ->
  gateway_util:false_check(Req:get(method) =:= ?POST, "request mothod error"),
  PostData = Req:parse_post(),
  gateway_util:signature_check(PostData),

  SessionId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  TowerID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("tower_id", PostData, undefined), "HintRequestDataError")),
  InscriptionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("inscription_id", PostData, undefined), "HintRequestDataError")),
  ReplaceID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("replaced_id", PostData, undefined), "HintRequestDataError")),
  InscriptionPos = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("pos", PostData, undefined), "HintRequestDataError")),
  ?FILE_LOG_DEBUG("replace Inscription=> session = ~p, tower = ~p, equipment_id = ~p, ReplaceID = ~p pos = ~p", [SessionId, TowerID, InscriptionID, ReplaceID, InscriptionPos]),

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, replace_inscription),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  case rpc:call(CacheNode, cache, replace_inscription, [Uin, TowerID, InscriptionID, ReplaceID, InscriptionPos]) of
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
      ?FILE_LOG_ERROR("replace_equipment proc error, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.