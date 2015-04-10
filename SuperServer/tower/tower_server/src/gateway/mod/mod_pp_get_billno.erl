%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 十二月 2014 下午3:10
%%%-------------------------------------------------------------------
-module(mod_pp_get_billno).
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
  GoodID  = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("goods_id", PostData, undefined), "HintRequestDataError")),

  ?FILE_LOG_INFO("pp_get_billno, session_id = ~p, goodsid = ~p", [SessionId, GoodID]),

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, pp_get_billno),

  {success, CacheNode} = gateway_util:get_cache_node(Uin),
  {success, HttpProcNode} = dd_ms:read_config(http_proc_node),
  case rpc:call(CacheNode, cache, get_user_info, [Uin, ["platform_info"]]) of
    {success, ValueList} ->
      PlatInfo =
        case ValueList of
          [{"player", _}, {"platform_info", PlatFormInfo}] -> PlatFormInfo;
          [{"platform_info", PlatFormInfo}, {"player", _}] -> PlatFormInfo
        end,
      if
        PlatInfo#platform_info.player_id =:= "" -> throw({custom, "UserNotLogin"});
        true -> ok
      end,
      case rpc:call(HttpProcNode, mod_pp, generate_orders, [PlatInfo#platform_info.player_id, GoodID, Uin]) of
        {success, {OrderID, Price}} ->
          Data =
            {
              struct,
              [
                {<<"bill_no">>, dd_util:to_binary(OrderID)},
                {<<"price">>, Price}
              ]
            },
          {
            struct,
            [
              {<<"result">>, 0},
              {<<"data">>, dd_util:to_binary(dd_util:encode_json_utf8(Data))},
              {<<"sign">>, gateway_util:back_signature(Data)}
            ]
          };
        {fail, _Reason} ->
          {
            struct,
            [
              {<<"result">>, -1},
              {<<"error">>, <<"HintSystemError">>}
            ]
          }
      end;
    {fail, Reason} ->
      ?FILE_LOG_ERROR("get user info error, reason = ~p", [Reason]),
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"error">>, <<"HintSystemError">>}
        ]
      }
  end.