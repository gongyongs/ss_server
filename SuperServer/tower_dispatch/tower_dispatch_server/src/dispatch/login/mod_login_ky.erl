%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 一月 2015 下午4:32
%%%-------------------------------------------------------------------
-module(mod_login_ky).
-author("zqlt").


-include("../dispatch.hrl").
-include("../../login/login.hrl").
-include("../../../deps/file_log/include/file_log.hrl").

-define(KY_VERIFY_URL, "http://f_signin.bppstore.com/loginCheck.php").
-define(KY_GAME_ID, "com.ogresugar.yumsaga.ky").
-define(KY_APP_KEY, "28e97502a7851eef3047582b3bf087f8").

%% API
-export([
  req_handle/1,
  login_verify_call_back/2
]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  Device = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("dev", PostData, undefined), "device not exists")),
  TokenKey = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("id", PostData, undefined), "id not exists")),

  Ip = Req:get(peer),
  {success, {UName, Plat, DisName}} = login_verify(TokenKey),
  {success, LoginNode} = dispatch_util:get_login_node(UName),

  ?FILE_LOG_DEBUG("ky login verify =>  uname = ~p, plat = ~p, disname = ~p", [UName, Plat, DisName]),
  case rpc:call(LoginNode, login_work, login_by_uname, [UName, login_ky, {UName, Plat, DisName, Device}]) of
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
      ?FILE_LOG_ERROR("login_ky error, reason = ~p", [Reason]),
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"error">>, <<"HintSystemError">>}
        ]
      };
    Other ->
      ?FILE_LOG_ERROR("login_ky error, reason = ~p", [Other]),
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


login_verify(TokenKey) when is_list(TokenKey) andalso length(TokenKey) > 0->
  ID = dd_util:timestamp(),
  MD5 = dd_util:md5_string(?KY_APP_KEY ++ TokenKey),
  ReqData = "tokenKey=" ++ TokenKey ++ "&sign=" ++ MD5,
  dispatch_util:http_post_req(?KY_VERIFY_URL, "application/x-www-form-urlencoded", ReqData, fun mod_login_ky:login_verify_call_back/2, ID).

login_verify_call_back(ReqData, _ID) ->
  {struct, JsonDataList} = mochijson2:decode(ReqData),
  Code = dd_util:to_integer(dispatch_util:get_json_value(<<"code">>, JsonDataList)),
  Msg = dd_util:to_list(dispatch_util:get_json_value(<<"msg">>, JsonDataList)),
  case Code of
    0 ->
      {struct, DataList} = dispatch_util:get_json_value(<<"data">>, JsonDataList),
      AccountID = dd_util:to_list(dispatch_util:get_json_value(<<"guid">>, DataList)),
      NickName = dd_util:to_list(dispatch_util:get_json_value(<<"username">>, DataList)),
      {success, {AccountID, "KY", NickName}};
    _ ->
      MsgJson = {struct, [{<<"reason">>, dd_util:to_binary(Msg)}]},
      Json = dd_util:encode_json_utf8(MsgJson),
      ?FILE_LOG_ERROR("KY LOGIN VERIFY: Request verify error: unknow error, error = ~p", [Json]),
      throw({custom, dd_util:to_binary(Msg)})
  end.
