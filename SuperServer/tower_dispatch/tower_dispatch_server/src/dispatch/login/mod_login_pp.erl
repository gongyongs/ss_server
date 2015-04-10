%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午4:12
%%%-------------------------------------------------------------------
-module(mod_login_pp).
-author("zqlt").


-include("../dispatch.hrl").
-include("../../login/login.hrl").
-include("../../../deps/file_log/include/file_log.hrl").

-define(PP_VERIFY_URL, "http://passport_i.25pp.com:8080/account?tunnel-command=2852126760").
-define(PP_GAME_ID, 4931).
-define(PP_APP_KEY, "7e228763ade8aba96e28c393a076c525").

%% API
-export([
  req_handle/1,
  login_verify_call_back/2
]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  Device = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("dev", PostData, undefined), "device not exists")),
  Sid = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("id", PostData, undefined), "id not exists")),

  Ip = Req:get(peer),
  {success, {UName, Plat, DisName}} = login_verify(Sid),
  {success, LoginNode} = dispatch_util:get_login_node(UName),

  ?FILE_LOG_DEBUG("pp login verify =>  uname = ~p, plat = ~p, disname = ~p", [UName, Plat, DisName]),
  case rpc:call(LoginNode, login_work, login_by_uname, [UName, login_pp, {UName, Plat, DisName, Device}]) of
    {success, UserInfo} ->
      LoginInfo = #login_user_info{ip = dd_util:ipv4_to_str(Ip), device = Device, uin = UserInfo#user_info.uin,
      plat_dis_name = UserInfo#user_info.dis_name, plat_id = UserInfo#user_info.uname, plat_type = UserInfo#user_info.platform_info#platform.plat_type},
      Token = generate_token(),
      {success, TokenNode} =  dd_config:get_cfg(token_node),
      {success, GameServerList} =
        case rpc:call(TokenNode, game_server_cache, get_game_server_list, [UserInfo#user_info.uin, Token, LoginInfo]) of
          {badrpc, BadReason} ->
            ?FILE_LOG_ERROR("badrpc reason ~p", [BadReason]),
            throw({custom, "HintSystemError"});
          Other -> Other
        end,
      %%组装json
      PlatType = string:to_lower(dd_util:to_list(Plat)),
      GameServerJsonList =
        lists:foldr(
          fun({_ServerID, ServerName, ServerType, {ServerAddr, ServerPort}, ServerState}, TmpList) ->
            ServerPlat = string:to_lower(dd_util:to_list(ServerType)),
            case PlatType of
              "" ->
                [{
                  struct,
                  [
                    {<<"display_info">>, {struct, [{<<"name">>, dd_util:to_binary(ServerName)}, {<<"type">>, dd_util:to_binary(ServerType)}, {<<"state">>, ServerState}]}},
                    {<<"server_info">>, {struct, [{<<"ip">>, dd_util:to_binary(ServerAddr)}, {<<"port">>, ServerPort}]}}
                  ]
                } | TmpList];
              ServerPlat ->
                [{
                  struct,
                  [
                    {<<"display_info">>, {struct, [{<<"name">>, dd_util:to_binary(ServerName)}, {<<"type">>, dd_util:to_binary(ServerType)}, {<<"state">>, ServerState}]}},
                    {<<"server_info">>, {struct, [{<<"ip">>, dd_util:to_binary(ServerAddr)}, {<<"port">>, ServerPort}]}}
                  ]
                } | TmpList];
              _ -> TmpList
            end
%%             [{
%%               struct,
%%               [
%%                 {<<"display_info">>, {struct, [{<<"name">>, dd_util:to_binary(ServerName)}, {<<"type">>, dd_util:to_binary(ServerType)}, {<<"state">>, ServerState}]}},
%%                 {<<"server_info">>, {struct, [{<<"ip">>, dd_util:to_binary(ServerAddr)}, {<<"port">>, ServerPort}]}}
%%               ]
%%             } | TmpList]
          end,[], GameServerList),

      Data =
        {
          struct,
          [
            {<<"login_info">>, {struct, [{<<"uin">>, UserInfo#user_info.uin}, {<<"token">>, dd_util:to_binary(Token)}, {<<"accountId">>, dd_util:to_binary(UserInfo#user_info.uname)}, {<<"dis_name">>, dd_util:to_binary(UserInfo#user_info.dis_name)}]}},
            {<<"server_list">>, GameServerJsonList}
          ]
        },

      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, Data}
        ]
      };
    {fail, Reason} ->
      ?FILE_LOG_ERROR("login_pp error, reason = ~p", [Reason]),
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"error">>, <<"HintSystemError">>}
        ]
      };
    Other ->
      ?FILE_LOG_ERROR("login_pp error, reason = ~p", [Other]),
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"error">>, <<"HintSystemError">>}
        ]
      }
  end.

generate_token() ->
  dd_util:to_list(dd_util:timestamp()).


login_verify(Sid) when is_list(Sid) andalso length(Sid) > 0->
  ID = dd_util:timestamp(),
  MD5 = dd_util:md5_string("sid=" ++ Sid ++ ?PP_APP_KEY),
  ReqData =
    {
      struct,
      [
        {<<"id">>, ID},
        {<<"service">>, <<"account.verifySession">>},
        {<<"data">>,{struct,[{<<"sid">>, dd_util:to_binary(Sid)}]}},
        {<<"game">>,{struct,[{<<"gameId">>, ?PP_GAME_ID}]}},
        {<<"encrypt">>, <<"md5">>},
        {<<"sign">>, dd_util:to_binary(MD5)}
      ]
    },
  Value = mochijson2:encode(ReqData),
  dispatch_util:http_post_req(?PP_VERIFY_URL, "application/json;charset=utf-8", Value,fun mod_login_pp:login_verify_call_back/2, ID).

login_verify_call_back(ReqData, ID) ->
  {struct, JsonDataList} = mochijson2:decode(ReqData),
  ReqID = dd_util:to_integer(dispatch_util:get_json_value(<<"id">>, JsonDataList)),
  if
    ID =:= ReqID -> ok;
    true ->
      ?FILE_LOG_ERROR("login_verify_call_back error=> id not match, req_id = ~p, id = ~p", [ReqID, ID]),
      throw({custom, "HintSystemError"})
  end,
  {struct, StateJsonList} = dispatch_util:get_json_value(<<"state">>, JsonDataList),
  Code = dd_util:to_integer(dispatch_util:get_json_value(<<"code">>, StateJsonList)),
  Msg = dd_util:to_list(dispatch_util:get_json_value(<<"msg">>, StateJsonList)),
  case Code of
    1 ->
      {struct, UserDataJsonList} = dispatch_util:get_json_value(<<"data">>, JsonDataList),
      AccountID = dd_util:to_list(dispatch_util:get_json_value(<<"accountId">>, UserDataJsonList)),
      Creator = dd_util:to_list(dispatch_util:get_json_value(<<"creator">>, UserDataJsonList)),
      NickName = dd_util:to_list(dispatch_util:get_json_value(<<"nickName">>, UserDataJsonList)),
      {success, {AccountID, Creator, NickName}};
    10 ->
      ?FILE_LOG_ERROR("PP LOGIN VERIFY: Request verify error: requst param error, ERROR = ~p", [Msg]),
      throw({custom, "HintSystemError"});
    11 ->
      ?FILE_LOG_ERROR("PP LOGIN VERIFY: Request verify error: user not login, error = ~p", [Msg]),
      throw({custom, "HintNotLoginPlatform"});
    99 ->
      ?FILE_LOG_ERROR("PP LOGIN VERIFY: Request verify error: network error, please retry, error = ~p", [Msg]),
      throw({custom, "HintNetworkError"});
    _ ->
      ?FILE_LOG_ERROR("PP LOGIN VERIFY: Request verify error: unknow error, error = ~p", [Msg]),
      throw({custom, "HintSystemError"})
  end.

