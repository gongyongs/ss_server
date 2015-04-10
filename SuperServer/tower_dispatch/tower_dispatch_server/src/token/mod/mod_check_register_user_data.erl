%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 九月 2014 下午6:32
%%%-------------------------------------------------------------------
-module(mod_check_register_user_data).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../login/login.hrl").
-export([req_handle/1]).

req_handle(Req) ->
  PostData = Req:parse_post(),
  Uin = dd_util:to_integer(proplists:get_value("uin", PostData, undefined)),
  ?FILE_LOG_DEBUG("check_register_user_data => uin = ~p", [Uin]),

  case game_server_request:check_register_user(Uin) of
    {success, {UserInfo, FriendData}} ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>,
            {
              struct,
              [
                {<<"uname">>, dd_util:to_binary(UserInfo#user_info.uname)},
                {<<"dis_name">>, dd_util:to_binary(UserInfo#user_info.dis_name)},
                {<<"device">>, dd_util:to_binary(UserInfo#user_info.device)},
                {<<"plat">>, dd_util:to_binary(UserInfo#user_info.platform_info#platform.plat_type)},
                {<<"friend">>, game_server_util:encode_json_request_friend_data(FriendData)}
              ]
            }
          }
        ]
      };
    {fail, Reason} ->
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"reason">>, dd_util:to_binary(Reason)}
        ]
      }
  end.
