%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 九月 2014 下午4:40
%%%-------------------------------------------------------------------
-module(mod_bond_device).
-author("zqlt").

-include("../dispatch.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-export([req_handle/1]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  Device = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("dev", PostData, undefined), "device not exists")),
  Plat = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("plat", PostData, undefined), "plat not exists")),
  PlayerID = dd_util:to_list(dispatch_util:undefined_with_default_value(proplists:get_value("id", PostData, undefined), "")),
  PlayerDisName = dd_util:to_list(dispatch_util:undefined_with_default_value(proplists:get_value("name", PostData, undefined), "")),
  FriendJson = dd_util:to_list(dispatch_util:undefined_with_default_value(proplists:get_value("friends", PostData, undefined), "{}")),

  ?FILE_LOG_DEBUG("bond_device => plat = ~p, device = ~p, playerid = ~p, disname = ~p, friend json = ~p", [Plat, Device, PlayerID, PlayerDisName, FriendJson]),

  dispatch_util:check_empty(Plat, "plat info is null"),
  dispatch_util:check_empty(PlayerID, "player id is null"),
  dispatch_util:check_empty(Device, "device is null"),

  Friend = dispatch_util:decode_json_friends(FriendJson),

  {success, Node} = dispatch_util:get_login_node(Device),
  case rpc:call(Node, login_cache, bond_device, [Plat, Device, PlayerID, PlayerDisName, Friend]) of
    {success, _UserInfo} ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"code">>, 0}     %%成功绑定
        ]
      };
    account_has_bond ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"code">>, 1}     %%已绑定
        ]
      };
    {fail, Reason} ->
      ?FILE_LOG_ERROR("bonde device, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.
