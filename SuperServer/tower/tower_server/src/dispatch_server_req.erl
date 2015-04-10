%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 九月 2014 下午7:27
%%%-------------------------------------------------------------------
-module(dispatch_server_req).
-author("zqlt").
-include("../deps/file_log/include/file_log.hrl").
-include("cache/cache_def.hrl").

%% API
-export([
  get_friend_data_list/1,
  sync_register_user_data/1
]).

-export([
  get_json_value_without_exception/3
]).

get_friend_data_list(Uin) ->
  {success, {DispatchSeverIP, DispatchServerPort}} = dd_ms:read_config(dispatch_server),
  Value = "uin=" ++ dd_util:to_list(Uin),
  ReqUrl = lists:flatten(["http://", DispatchSeverIP, ":", dd_util:to_list(DispatchServerPort), "/request_friend_data"]),
  ReqValue = httpc:request('post', {ReqUrl, [], "application/x-www-form-urlencoded", iolist_to_binary(Value)}, [], []),
  {struct, JsonValue} = mochijson2:decode(ReqValue),
  Code = dd_util:to_integer(get_json_value(<<"result">>, JsonValue)),
  case Code of
    0 ->
      DataJsonList = get_json_value(<<"data">>, JsonValue),
      FriendList =
        lists:map(
          fun(Item) ->
            FriendUin = dd_util:to_integer(get_json_value(<<"uin">>, Item)),
            UName = dd_util:to_list(get_json_value(<<"uname">>, Item)),
            DisName = dd_util:to_list(get_json_value(<<"dis_name">>, Item)),
            {FriendUin, UName, DisName}
          end, DataJsonList),
      {success, FriendList};
    _ ->
      Reason = dd_util:to_list(get_json_value(<<"reason">>, JsonValue)),
      {fail, Reason}
  end.

sync_register_user_data(Uin) when is_integer(Uin) ->
  {success, {DispatchSeverIP, DispatchServerPort}} = dd_ms:read_config(dispatch_server),
  Value = "uin=" ++ dd_util:to_list(Uin),
  ReqUrl = lists:flatten(["http://", DispatchSeverIP, ":", dd_util:to_list(DispatchServerPort), "/check_register_user_data"]),
  ReqValue = httpc:request('post', {ReqUrl, [], "application/x-www-form-urlencoded", iolist_to_binary(Value)}, [], []),
  ?FILE_LOG_DEBUG("REq value : ~p", [ReqValue]),
  case ReqValue of
    {ok, {{_,Code,_}, _,  Body}} ->
      StatusCode = dd_util:to_integer(Code),
      if
        StatusCode =/= 200 ->
          ?FILE_LOG_ERROR("sync register user data error, status code = ~p", [StatusCode]),
          {fail, "logic error"};
        true ->
          parse_sync_register_data(Body)
      end;
    {ok, {{_,Code,_}, Body}} ->
      StatusCode = dd_util:to_integer(Code),
      if
        StatusCode =/= 200 ->
          ?FILE_LOG_ERROR("sync register user data error, status code = ~p", [StatusCode]),
          {fail, "logic error"};
        true ->
          parse_sync_register_data(Body)
      end;
    {error, Reason} ->
      ?FILE_LOG_ERROR("sync register user data error, reason = ~p", [Reason]),
      {fail, Reason};
    Other ->
      ?FILE_LOG_ERROR("sync register user data error,  ~p", [Other]),
      {fail, "logic error"}
  end.



parse_sync_register_data(Data) ->
  {struct, JsonValue} = mochijson2:decode(Data),
  Code = dd_util:to_integer(get_json_value(<<"result">>, JsonValue)),
  case Code of
    0 ->
      {struct, DataJsonList} = get_json_value(<<"data">>, JsonValue),
      UName = dd_util:to_list(get_json_value(<<"uname">>, DataJsonList)),
      DisName = dd_util:to_list(get_json_value(<<"dis_name">>, DataJsonList)),
      Device = dd_util:to_list(get_json_value(<<"device">>, DataJsonList)),
      Plat = dd_util:to_list(get_json_value(<<"plat">>, DataJsonList)),
      FriendJsonList = get_json_value(<<"friend">>, DataJsonList),
      FriendList =
        lists:foldr(
          fun({struct,Item}, TmpList) ->
            FriendUin = dd_util:to_integer(get_json_value(<<"uin">>, Item)),
            FriendUName = dd_util:to_list(get_json_value(<<"uname">>, Item)),
            FriendDisName = dd_util:to_list(get_json_value(<<"dis_name">>, Item)),
            if
              FriendUName =:= UName -> TmpList;
              true ->
                [#friend_item{uin = FriendUin, dis_name = FriendDisName, id = FriendUName} | TmpList]
            end
          end, [], FriendJsonList),
      {success, {UName, DisName, Device, Plat, FriendList}};
    _ ->
      Reason = dd_util:to_list(get_json_value(<<"reason">>, JsonValue)),
      {fail, Reason}
  end.

get_json_value(Key, PropList) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      throw({custom, "error json key" ++ dd_util:to_list(Key)});
    Value -> Value
  end.

get_json_value_without_exception(Key, PropList, DefaultValue) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      DefaultValue;
    Value -> Value
  end.
