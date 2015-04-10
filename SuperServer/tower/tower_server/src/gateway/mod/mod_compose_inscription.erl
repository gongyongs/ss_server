%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 一月 2015 上午10:53
%%%-------------------------------------------------------------------
-module(mod_compose_inscription).
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
  InscriptionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("object_id", PostData, undefined), "HintRequestDataError")),

  ?FILE_LOG_DEBUG("mod_compose_inscription => [session = ~p, object_id =  ~p]", [SessionId, InscriptionID]),

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, strengthen),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  case rpc:call(CacheNode, cache, compose_inscription, [Uin, InscriptionID])  of
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
