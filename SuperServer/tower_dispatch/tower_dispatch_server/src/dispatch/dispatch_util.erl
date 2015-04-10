%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 七月 2014 上午9:42
%%%-------------------------------------------------------------------
-module(dispatch_util).
-author("zqlt").
-include("dispatch.hrl").
-include("../dd_ms.hrl").
-include("../login/login.hrl").
-include("../../deps/file_log/include/file_log.hrl").

-define(VERSION_RE, "^[a-zA-Z0-9_.-]{0,500}$").

%% API
-export([
  undefined_check/2,
  false_check/2,
  get_login_node/1,
  undefined_with_default_value/2,
  check_empty/2,
  http_post_req/5,
  http_get_req/3,
  get_json_value/2,
  check_string_valid/1,
  get_config_param/1
]).

-export([
  decode_json_friends/1
]).

check_empty(Value, Reason) ->
  param_check(Value, [], Reason).

undefined_with_default_value(Value, Default) ->
  undefined_with_default(Value, Default).
undefined_with_default(undefined, Default) -> Default;
undefined_with_default(Value, _) -> Value.


undefined_check(Value, Reason) ->
  param_check(Value, undefined, Reason).

false_check(Value, Reason) ->
  param_check(Value, false, Reason).


param_check(Value, Value, Reason) ->
  throw({custom, Reason});
param_check(Value, _, _) -> Value.

%%暂时采用最简单的哈希算法
get_login_node(Key) when is_list(Key) ->
  {success, LoginHashRule} = get_config_param(login_hash_rule),
  io:format("now in LoginHashRule --->~p~n",[LoginHashRule]),
  case hash_service_util:find_key_store_node(Key, LoginHashRule) of
    fail -> throw({custom, "not available data nodes 2"});
    {success, Node} -> io:format("now in Node is  --->~p~n",[Node]),{success, Node}
  end.

get_config_param(Key) ->
  case dd_config:get_cfg(Key) of
    {success, Value} -> {success, Value};
    fail ->
      ?FILE_LOG_WARNING("get_key[~p] fail", [Key]),
      throw({custom, "get_config_param fail"})
  end.

decode_json_friends(FriendJson) ->
  {struct, FriendJsonList} = mochijson2:decode(FriendJson),
  FriendInfoJsonList = get_json_value_without_exception(<<"friend">>, FriendJsonList, []),
  FriendList =
    lists:map(
      fun({struct, FriendItem}) ->
        ID = dd_util:to_list(get_json_value(<<"id">>, FriendItem)),
        DisName = dd_util:to_list(get_json_value(<<"name">>, FriendItem)),
        #friend_item{id = ID, dis_name = DisName}
      end, FriendInfoJsonList),
  #friends{friend_list = FriendList}.


get_json_value(Key, PropList) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      throw({custom, "error json key" ++ dd_util:to_list(Key)});
    Value -> Value
  end.

get_json_value_without_exception(Key, PropList, DefaultValue) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined -> DefaultValue;
    Value -> Value
  end.

%%options 是传入参数，不做任何修改直接传出
http_post_req(Url, Pattern, PostData, CallbackFunc, Options) when is_list(Url) andalso is_list(Pattern) andalso is_function(CallbackFunc)->
  ReqValue = httpc:request('post', {Url, [], Pattern, iolist_to_binary(PostData)}, [], []),
  case ReqValue of
    {ok, {{_,Code,_}, _,  Body}} ->
      StatusCode = dd_util:to_integer(Code),
      case StatusCode of
        200 -> CallbackFunc(Body, Options);
        _ -> throw({custom, "HTTP POST Request error"})
      end;
    {ok, {{_,Code,_}, Body}} ->
      StatusCode = dd_util:to_integer(Code),
      case StatusCode of
        200 -> CallbackFunc(Body, Options);
        _ -> throw({custom, "HTTP POST Request error"})
      end;
    {error, Reason} -> throw({custom, Reason});
    Other -> throw({custom, Other})
  end.

http_get_req(Url, CallbackFunc, Options) when is_list(Url) andalso is_function(CallbackFunc)->
  ReqValue = httpc:request('get', {Url, []}, [], []),
  case ReqValue of
    {ok, {{_,Code,_}, _,  Body}} ->
      StatusCode = dd_util:to_integer(Code),
      case StatusCode of
        200 -> CallbackFunc(Body, Options);
        _ -> throw({custom, "HTTP GET Request error"})
      end;
    {ok, {{_,Code,_}, Body}} ->
      StatusCode = dd_util:to_integer(Code),
      case StatusCode of
        200 -> CallbackFunc(Body, Options);
        _ -> throw({custom, "HTTP GET Request error"})
      end;
    {error, Reason} -> throw({custom, Reason});
    Other -> throw({custom, Other})
  end.

check_string_valid(Str) when is_list(Str) ->
  {ok, Re} = re:compile(?VERSION_RE),
  case re:run(Str, Re) of
    nomatch ->
      ?FILE_LOG_ERROR("invalid string re, str = ~p", [Str]),
      throw({custom, "HintInvalidRequestData"});
    {match, _} -> ok
  end.
