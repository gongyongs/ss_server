%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 一月 2015 下午8:55
%%%-------------------------------------------------------------------
-module(mod_login_tb).
-author("zqlt").

-include("../dispatch.hrl").
-include("../../login/login.hrl").
-include("../../../deps/file_log/include/file_log.hrl").


-define(TB_VERIFY_URL, "http://tgi.tongbu.com/checkv2.aspx").

%% API
-export([
  req_handle/1,
  login_verify_call_back/2
]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  LoginType = dd_util:to_integer(dispatch_util:undefined_check(proplists:get_value("tp", PostData, undefined), "login type not exists")),
  Device = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("dev", PostData, undefined), "device not exists")),
  Plat = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("plat", PostData, undefined), "plat not exists")),
  SessionID = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("id", PostData, undefined), "player id not exits")),
  PlayerDisName = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("name", PostData, undefined), "player name not exist")),

  Ip = Req:get(peer),

  {success, PlayerID} = login_verify(SessionID),
  {success, LoginNode} = dispatch_util:get_login_node(PlayerID),

  ?FILE_LOG_DEBUG("TB login ==> login_type = ~p, plat = ~p, device = ~p, uname =~p, dis_name = ~p, session = ~p", [LoginType,Plat, Device, PlayerID, PlayerDisName, SessionID]),


  case rpc:call(LoginNode, login_work, login_by_uname, [PlayerID, login_tb, {PlayerID, "TB", PlayerDisName, Device}]) of
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


login_verify(Token) when is_list(Token) ->
  TBUrl = ?TB_VERIFY_URL ++ "?k=" ++ Token,
  dispatch_util:http_get_req(TBUrl, fun mod_login_tb:login_verify_call_back/2, 0).

login_verify_call_back(ReqData, _) ->
  Data = dd_util:to_list(ReqData),
  case Data of
    "0" -> %%失效；
      ?FILE_LOG_ERROR("tbt login verify token is invalid", []),
      throw({custom, "HintSystemDataError"});
    "-1" -> %%格式错误
      ?FILE_LOG_ERROR("tbt login verify token format is invalid", []),
      throw({custom, "HintSystemDataError"});
    "" -> %%验证错误
      ?FILE_LOG_DEBUG("tbt login verify token fail, null", []),
      throw({custom, "HintSystemDataError"});
    UName -> %%正确
      ?FILE_LOG_DEBUG("tbt login verify token success, uname = ~p", [UName]),
      {success, UName}
  end.
