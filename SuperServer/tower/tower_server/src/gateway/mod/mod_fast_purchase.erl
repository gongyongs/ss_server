%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 九月 2014 下午3:19
%%%-------------------------------------------------------------------
-module(mod_fast_purchase).
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

  %%type: 1体力，2金币，3背包容量
  SessionId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  PurchaseType = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("type", PostData, undefined), "HintRequestDataError")),
  Value = dd_util:to_list(gateway_util:undefined_with_default_value(proplists:get_value("value", PostData, undefined), "")),

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, fast_purchase),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  ?FILE_LOG_DEBUG("mod_fast_purchase =>  session_id = ~p, purchase_type = ~p, value = ~p, uin = ~p", [SessionId, PurchaseType, Value, Uin]),
  ?FILE_LOG_INFO("fast_purchase: [~p][~p][~p]", [Uin, PurchaseType, Value]),

  case rpc:call(CacheNode, cache, fast_purchase, [Uin, PurchaseType, Value]) of
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
