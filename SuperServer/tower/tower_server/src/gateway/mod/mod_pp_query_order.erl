%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 十二月 2014 下午3:11
%%%-------------------------------------------------------------------
-module(mod_pp_query_order).
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
  BillNo = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("bill_no", PostData, undefined), "HintRequestDataError")),

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, pp_query_order),

  {success, CacheNode} = gateway_util:get_cache_node(Uin),
  {success, HttpProcNode} = dd_ms:read_config(http_proc_node),
  case rpc:call(CacheNode, cache, get_user_info, [Uin, ["platform_info", "shop", "mission"]]) of
    {success, ValueList} ->
      F = fun({Key, _Value}, ID) -> Key =:= ID end,
      {success, {"platform_info", PlatInfo}} = cache_util:find_item_by_id(ValueList, F, "platform_info"),
      {success, {"player", {Uin, _DisName, Gold, Gem, Strength}}} = cache_util:find_item_by_id(ValueList, F, "player"),
      {success, {"shop", Shop}} = cache_util:find_item_by_id(ValueList, F, "shop"),
      {success, {"mission", Mission}} = cache_util:find_item_by_id(ValueList, F, "mission"),
      if
        PlatInfo#platform_info.player_id =:= "" -> throw({custom, "UserNotLogin"});
        true -> ok
      end,
      case rpc:call(HttpProcNode, mod_pp, query_order, [PlatInfo#platform_info.player_id, BillNo]) of
        {success, State, UpdateMission} ->
          Data =
            {
              struct,
              [
                {<<"state">>, State},
                {<<"player">>,
                  {
                    struct,
                    [
                      {<<"gold">>, Gold},
                      {<<"gem">>, Gem},
                      {<<"strength">>, Strength#strength.strength},
                      {<<"strength_cd">>,  gateway_util:get_strength_remain_time(Strength)}
                    ]
                  }
                },
                {<<"shop">>, gateway_util:encode_json_shop(Shop)},
                {<<"update_mission">>, gateway_util:encode_json_mission(UpdateMission, Mission#mission.player_activity)}
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
        {success, State} ->
          Data =
            {
              struct,
              [
                {<<"state">>, State}
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
