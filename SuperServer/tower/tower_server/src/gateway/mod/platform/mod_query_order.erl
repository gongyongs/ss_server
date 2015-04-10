%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午4:57
%%%-------------------------------------------------------------------
-module(mod_query_order).
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
  BillNo = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("bill_no", PostData, undefined), "HintRequestDataError")),
  Plat = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("plat", PostData, undefined), "HintRequestDataError")),

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, query_order),
  PlatType = string:to_lower(Plat),
  platform_pay_util:check_plat(PlatType),

  {success, CacheNode} = gateway_util:get_cache_node(Uin),
  {success, HttpProcNode} = dd_ms:read_config(http_proc_node),
  case rpc:call(CacheNode, cache, query_account, [Uin, ["platform_info"]]) of
    {success, ValueList} ->
      PlatInfo = proplists:get_value("platform_info", ValueList),
      if
        PlatInfo#platform_info.player_id =:= "" -> throw({custom, "HintReLogin"});
        true -> ok
      end,
      ModName = erlang:list_to_atom("mod_" ++ PlatType),
      case rpc:call(HttpProcNode, ModName, query_order, [PlatInfo#platform_info.player_id, BillNo]) of
        {success, State, UpdateData} ->
          {struct, DataList} = mochijson2:decode(UpdateData),
          Data =
            {
              struct,
              [
                {<<"state">>, State} | DataList
              ]
            },
          Json = dd_util:encode_json_utf8(Data),
          {
            struct,
            [
              {<<"result">>, 0},
              {<<"data">>, dd_util:to_binary(Json)},
              {<<"sign">>, gateway_util:back_signature(Json)}
            ]
          };
        {success, State} ->
          Data =
            {
              struct,
              [
                {<<"state">>, State}
              ]
            },
          Json = dd_util:encode_json_utf8(Data),
          {
            struct,
            [
              {<<"result">>, 0},
              {<<"data">>, dd_util:to_binary(Json)},
              {<<"sign">>, gateway_util:back_signature(Json)}
            ]
          };
        {fail, Reason} ->
          {
            struct,
            [
              {<<"result">>, -1},
              {<<"error">>, dd_util:to_binary(Reason)}
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
