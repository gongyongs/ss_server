%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 八月 2014 下午2:09
%%%-------------------------------------------------------------------
-module(mod_buy_goods).
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
  CommodityId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("commodity_id", PostData, undefined), "HintRequestDataError")),

  Ip = Req:get(peer),
  ?FILE_LOG_DEBUG("mod_buy_goods =>  session_id = ~p, commodity_id = ~p, ip = ~p", [SessionId, CommodityId, Ip]),

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, buy_goods),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  case rpc:call(CacheNode, cache, buy_goods, [Uin, CommodityId, Ip]) of
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
