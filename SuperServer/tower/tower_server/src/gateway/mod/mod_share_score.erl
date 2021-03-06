%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 九月 2014 下午9:16
%%%-------------------------------------------------------------------
-module(mod_share_score).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST),"request method error"),
  PostData = Req:parse_post(),
  gateway_util:signature_check(PostData),

  SessionId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  PlatName = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("plat", PostData, undefined), "HintRequestDataError")),
  {success, Uin} = gateway_util:get_uin_by_session(SessionId, share_score),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  ?FILE_LOG_DEBUG("get_endless_germ =>  session_id = ~p, Uin = ~p, plat = ~p", [SessionId, Uin, PlatName]),

  case rpc:call(CacheNode, cache, share_score, [Uin, PlatName]) of
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
      ?FILE_LOG_ERROR("buy_goods error, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.
