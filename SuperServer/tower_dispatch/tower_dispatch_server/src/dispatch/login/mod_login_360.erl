%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午4:12
%%%-------------------------------------------------------------------
-module(mod_login_360).
-author("zqlt").


-include("../dispatch.hrl").
-include("../../login/login.hrl").
-include("../../../deps/file_log/include/file_log.hrl").

-define(VERIFY_URL_360, "https://openapi.360.cn/user/me.json?").
-define(GAME_ID_360, 202219501).
-define(APP_KEY_360, "abcb5a7ab165ad4d356b6e9194690430").
-define(APP_SECRET_360, "2ea259f2060af1b9c451974f978993b7").

%% API
-export([
  req_handle/1,
  login_verify_call_back/2
]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  Device = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("dev", PostData, undefined), "device not exists")),
  AccessToken = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("id", PostData, undefined), "id not exists")),  %%access_token

  Ip = Req:get(peer),
  {success, {UName, Plat, DisName}} = login_verify(AccessToken),
  {success, LoginNode} = dispatch_util:get_login_node(UName),

  ?FILE_LOG_DEBUG("360 login verify =>  uname = ~p, plat = ~p, disname = ~p", [UName, Plat, DisName]),
  case rpc:call(LoginNode, login_work, login_by_uname, [UName, login_360, {UName, Plat, DisName, Device}]) of
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
      ?FILE_LOG_ERROR("login_360 error, reason = ~p", [Reason]),
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"error">>, <<"HintSystemError">>}
        ]
      };
    Other ->
      ?FILE_LOG_ERROR("login_360 error, reason = ~p", [Other]),
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


login_verify(AccessToken) when is_list(AccessToken) andalso length(AccessToken) > 0->
  ReqUrl =
    ?VERIFY_URL_360 ++
    "access_token=" ++ AccessToken ++
    "&fields=id,name,nick",
  dispatch_util:http_get_req(ReqUrl, fun mod_login_360:login_verify_call_back/2, "").

login_verify_call_back(ReqData, _) ->
  {struct, JsonDataList} = mochijson2:decode(ReqData),
  UserID = dd_util:to_list(dispatch_util:get_json_value(<<"id">>, JsonDataList)),
  UserName = dd_util:to_list(dispatch_util:get_json_value(<<"name">>, JsonDataList)),
  NickName = dd_util:to_list(dispatch_util:get_json_value(<<"nick">>, JsonDataList)),
  DisName =
    case NickName of
      "" -> UserName;
      _ -> NickName
    end,
  {success, {UserID, "360", DisName}}.
