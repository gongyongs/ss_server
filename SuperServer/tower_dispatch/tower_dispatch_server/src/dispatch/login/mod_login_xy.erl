%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 一月 2015 上午11:44
%%%-------------------------------------------------------------------
-module(mod_login_xy).
-author("zqlt").


-include("../dispatch.hrl").
-include("../../login/login.hrl").
-include("../../../deps/file_log/include/file_log.hrl").

-define(XY_VERIFY_URL, "http://passport.xyzs.com/checkLogin.php").
-define(XY_GAME_ID, 123).
-define(XY_APP_KEY, "123").

%% API
-export([
  req_handle/1,
  login_verify_call_back/2
]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  Device = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("dev", PostData, undefined), "device not exists")),
  UName = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("id", PostData, undefined), "id not exists")),
  DisName = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("name", PostData, undefined), "name not exist")),
  Token = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("name", PostData, undefined), "token not exist")),
  Plat = "XY",
  Ip = Req:get(peer),
  login_verify(UName, Token),
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


login_verify(Sid, Token) when is_list(Sid) andalso is_list(Token) ->
  Value = "uid=" ++ dd_util:to_list(Sid) ++ "&appid" ++ dd_util:to_list(?XY_GAME_ID) ++ "&token=" ++ dd_util:to_list(Token),
  dispatch_util:http_post_req(?XY_VERIFY_URL, "application/x-www-form-urlencoded", Value, fun mod_login_xy:login_verify_call_back/2, Sid).

login_verify_call_back(ReqData, Sid) ->
  {struct, JsonDataList} = mochijson2:decode(ReqData),
  Ret = dd_util:to_integer(dispatch_util:get_json_value(<<"ret">>, JsonDataList)),
  case Ret of
    0 ->
      ?FILE_LOG_DEBUG("xy user success login, uid = ~p", [Sid]),
      success;
    Other ->
      Error = dd_util:to_list(dispatch_util:get_json_value(<<"error">>, JsonDataList)),
      ?FILE_LOG_ERROR("XY LOGIN VERIFY: Request verify error: requst param error,code = ~p, ERROR = ~p", [Other, unicode:characters_to_binary(Error)]),
      throw({custom, Error})
  end.
