%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 七月 2014 下午5:53
%%%-------------------------------------------------------------------
-module(mod_register).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../dispatch.hrl").
-include("../../login/login.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
   dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
   PostData = Req:parse_post(),
  Plat = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("plat", PostData, undefined), "plat not exists")),
  PlayerID = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("uname", PostData, undefined), "uname not not exists")),
  DisName = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("dis_name", PostData, undefined), "dis_name not not exists")),
  FriendJson = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("friends", PostData, undefined), "friend not not exists")),

  ?FILE_LOG_DEBUG("login ==> plat = ~p, uname =~p, dis_name = ~p, friendjson = ~p ", [Plat, PlayerID, DisName, FriendJson]),

  Friend = dispatch_util:decode_json_friends(FriendJson),

  {success, Node} = dispatch_util:get_login_node(PlayerID),

  Uin =
    case rpc:call(Node, login_work, execute, [login_ios, register, {Plat, PlayerID, DisName, Friend}]) of
      {success, UserInfo} -> UserInfo#user_info.uin;
      {fail, FailReason} -> throw({custom, FailReason});
      {badrpc, Reason} ->
        ?FILE_LOG_ERROR("badrpc reason=~p", [Reason]),
        throw({custom, "sys error"})
    end,

  {
    struct,
    [
      {<<"result">>, 0},
      {<<"data">>, {struct, [{<<"uin">>, Uin}]}}
    ]
  }.


