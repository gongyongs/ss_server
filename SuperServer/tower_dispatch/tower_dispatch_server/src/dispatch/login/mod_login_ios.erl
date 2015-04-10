%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午4:12
%%%-------------------------------------------------------------------
-module(mod_login_ios).
-author("zqlt").

-include("../dispatch.hrl").
-include("../../login/login.hrl").
-include("../../../deps/file_log/include/file_log.hrl").

%% API
-export([req_handle/1]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  LoginType = dd_util:to_integer(dispatch_util:undefined_check(proplists:get_value("tp", PostData, undefined), "login type not exists")),
  Device = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("dev", PostData, undefined), "device not exists")),
  Plat = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("plat", PostData, undefined), "plat not exists")),
  PlayerID = dd_util:to_list(dispatch_util:undefined_with_default_value(proplists:get_value("id", PostData, undefined), "")),
  PlayerDisName = dd_util:to_list(dispatch_util:undefined_with_default_value(proplists:get_value("name", PostData, undefined), "")),
  FriendJson = dd_util:to_list(dispatch_util:undefined_with_default_value(proplists:get_value("friend", PostData, undefined), "{}")),

  Ip = Req:get(peer),

  {success, LoginNode} = dispatch_util:get_login_node(Device),

  ?FILE_LOG_DEBUG("login ==> login_type = ~p, plat = ~p, device = ~p, uname =~p, dis_name = ~p, friendjson = ~p ", [LoginType,Plat, Device, PlayerID, PlayerDisName, FriendJson]),

  Friend = dispatch_util:decode_json_friends(FriendJson),


  LoginResult =
    case LoginType of
      0 -> %%游客登录
        rpc:call(LoginNode,login_work, login_by_device, [Device, login_ios, Device]);
      1 -> %%平台登录
        rpc:call(LoginNode, login_work, login_by_uname, [Device, login_ios, {Plat, Device, PlayerID, PlayerDisName, Friend}])
    end,
  io:format("now in mod_login_ios LoginResult is  --->~p~n",[LoginResult]),
  Uin =
    case LoginResult of
      {success, UserInfo} ->  UserInfo#user_info.uin;
      {fail, FailReason} -> throw({fail, FailReason});
      {badrpc, Reason} ->
        ?FILE_LOG_ERROR("badrpc reason = ~p", [Reason]),
        throw({custom, "sys error"})
    end,
  LoginInfo = #login_user_info{ip = dd_util:ipv4_to_str(Ip), device = Device, uin = Uin, plat_dis_name = PlayerDisName, plat_id = PlayerID, plat_type = Plat},
  Token = generate_token(),

  {success, TokenNode} =  dd_config:get_cfg(token_node),
  {success, GameServerList} =
    case rpc:call(TokenNode, game_server_cache, get_game_server_list, [Uin, Token, LoginInfo]) of
      {badrpc, BadReason} ->
        ?FILE_LOG_ERROR("badrpc reason ~p", [BadReason]),
        throw({custom, "HintSystemError"});
      Other -> Other
    end,

  %%组装json
  GameServerJsonList =
    lists:foldr(
      fun({_ServerID, ServerName, ServerType, {ServerAddr, ServerPort}, ServerState}, TmpList) ->
%%         case Plat of
%%           "" ->
%%             [{
%%               struct,
%%               [
%%                 {<<"display_info">>, {struct, [{<<"name">>, dd_util:to_binary(ServerName)}, {<<"type">>, dd_util:to_binary(ServerType)}, {<<"state">>, ServerState}]}},
%%                 {<<"server_info">>, {struct, [{<<"ip">>, dd_util:to_binary(ServerAddr)}, {<<"port">>, ServerPort}]}}
%%               ]
%%             } | TmpList];
%%           ServerType ->
%%             [{
%%               struct,
%%               [
%%                 {<<"display_info">>, {struct, [{<<"name">>, dd_util:to_binary(ServerName)}, {<<"type">>, dd_util:to_binary(ServerType)}, {<<"state">>, ServerState}]}},
%%                 {<<"server_info">>, {struct, [{<<"ip">>, dd_util:to_binary(ServerAddr)}, {<<"port">>, ServerPort}]}}
%%               ]
%%             } | TmpList];
%%           _ -> TmpList
%%         end
        [{
          struct,
          [
            {<<"display_info">>, {struct, [{<<"name">>, dd_util:to_binary(ServerName)}, {<<"type">>, dd_util:to_binary(ServerType)}, {<<"state">>, ServerState}]}},
            {<<"server_info">>, {struct, [{<<"ip">>, dd_util:to_binary(ServerAddr)}, {<<"port">>, ServerPort}]}}
          ]
        } | TmpList]
      end,[], GameServerList),

  Data =
    {
      struct,
      [
        {<<"login_info">>, {struct, [{<<"uin">>, Uin}, {<<"token">>, dd_util:to_binary(Token)}]}},
        {<<"server_list">>, GameServerJsonList}
      ]
    },

  {
    struct,
    [
      {<<"result">>, 0},
      {<<"data">>, Data}
    ]
  }.


generate_token() ->
  dd_util:to_list(dd_util:timestamp()).