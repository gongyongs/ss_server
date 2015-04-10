%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午4:57
%%%-------------------------------------------------------------------
-module(mod_get_billno).
-author("zqlt").

-include("../../../../deps/file_log/include/file_log.hrl").
-include("../../gateway.hrl").
-include("../../../cache/cache_def.hrl").

%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST),"request method error"),
  PostData = Req:parse_post(),

  gateway_util:signature_check(PostData),

  SessionId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  GoodID  = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("goods_id", PostData, undefined), "HintRequestDataError")),
  Plat = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("plat", PostData, undefined), "HintRequestDataError")),

  ?FILE_LOG_INFO("get_billno, session_id = ~p, goodsid = ~p, plat = ~p", [SessionId, GoodID, Plat]),
  PlatType = string:to_lower(Plat),
  platform_pay_util:check_plat(PlatType),

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, get_billno),

  {success, CacheNode} = gateway_util:get_cache_node(Uin),
  {success, HttpProcNode} = dd_ms:read_config(http_proc_node),
  case rpc:call(CacheNode, cache, query_account, [Uin, ["platform_info"]]) of
    {success, ValueList} ->
      PlatInfo = proplists:get_value("platform_info", ValueList),
      case PlatInfo#platform_info.player_id of
        "" -> throw({custom, "UserNotLogin"});
        _ -> ok
      end,
      ModName = erlang:list_to_atom("mod_" ++ PlatType),
      ?FILE_LOG_DEBUG("get bill no , modname = ~p", [ModName]),
      case rpc:call(HttpProcNode, ModName, generate_orders, [PlatInfo#platform_info.player_id, GoodID, Uin]) of
        {success, {OrderID, Price, NotifyPath}} ->
          Data =
            {
              struct,
              [
                {<<"bill_no">>, dd_util:to_binary(OrderID)},
                {<<"price">>, dd_util:to_binary(Price)},
                {<<"call_back">>, dd_util:to_binary(NotifyPath)}
              ]
            },
          {
            struct,
            [
              {<<"result">>, 0},
              {<<"data">>, dd_util:to_binary(dd_util:encode_json_utf8(Data))},
              {<<"sign">>, gateway_util:back_signature(dd_util:encode_json_utf8(Data))}
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