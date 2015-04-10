%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 九月 2014 下午4:55
%%%-------------------------------------------------------------------
-module(mod_request_friend_data).
-author("zqlt").

-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  PostData = Req:parse_post(),
  Uin = dd_util:to_integer(proplists:get_value("uin", PostData, undefined)),
  ?FILE_LOG_DEBUG("request friend data => uin = ~p", [Uin]),

  case game_server_request:request_friend_data(Uin) of
    {success, List} ->
      FriendDataJsonList = game_server_util:encode_json_request_friend_data(List),
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, FriendDataJsonList}
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