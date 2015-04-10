%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 九月 2014 下午5:10
%%%-------------------------------------------------------------------
-module(game_server_util).
-author("zqlt").

%% API
-export([
  encode_json_request_friend_data/1
]).

encode_json_request_friend_data(FriendDataList) when is_list(FriendDataList) ->
    lists:map(
      fun({Uin, UName, DisName}) ->
        {
          struct,
          [
            {<<"uin">>, Uin},
            {<<"uname">>, dd_util:to_binary(UName)},
            {<<"dis_name">>, dd_util:to_binary(DisName)}
          ]
        }
      end, FriendDataList).