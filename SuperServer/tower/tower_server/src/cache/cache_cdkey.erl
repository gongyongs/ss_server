%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十二月 2014 下午9:38
%%%-------------------------------------------------------------------
-module(cache_cdkey).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
-include("cache_cdkey.hrl").
%% API
-export([
  query_cdkey_by_id/1,
  exchange_cdkey/1
]).

-export([
  get_json_value/2
]).

query_cdkey_by_id(CodeID) when is_list(CodeID) ->
  try
    {success, Url} = dd_ms:read_config(cdkey_url),
    http_post_req(dd_util:to_list(Url) ++ "query_cdkey", "application/x-www-form-urlencoded", "cdkey="++dd_util:to_list(CodeID),fun query_cdkey_call_back/2, [])
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_ERROR("query cdkey error, reason = ~p", [Reason]),
      fail;
    What:Type ->
      ?FILE_LOG_ERROR("query cdkey error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      fail
  end.

query_cdkey_call_back(ResultJson, _) ->
  {struct, JsonDataList} = mochijson2:decode(ResultJson),
  Result = dd_util:to_integer(get_json_value(<<"result">>, JsonDataList)),
  if
    Result =/= -1  -> ok;
    true ->
      ?FILE_LOG_ERROR("query_cdkey_call_back error", []),
      throw({custom, "HintInvalidCDKEY"})
  end,
  Status = dd_util:to_integer(get_json_value(<<"status">>, JsonDataList)),
  case Status of
    -1 -> %%不存在
      not_exist;
    1 ->  %%已兑换
      has_exchanged;
    2 ->  %%已失效
      invalid;
    0 ->
      {struct, CDKeyJsonList} = get_json_value(<<"cdkey">>, JsonDataList),
      {success, decode_cdkey_json(CDKeyJsonList)}
  end.

exchange_cdkey(CodeID) when is_list(CodeID) ->
  try
    {success, Url} = dd_ms:read_config(cdkey_url),
    http_post_req(dd_util:to_list(Url) ++ "exchange_cdkey", "application/x-www-form-urlencoded", "cdkey="++dd_util:to_list(CodeID),fun exchange_cdkey_call_back/2, [])
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_ERROR("exchange_cdkey error, reason = ~p", [Reason]),
      fail;
    What:Type ->
      ?FILE_LOG_ERROR("exchange_cdkey error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      fail
  end.

exchange_cdkey_call_back(ResultJson, _) ->
  {struct, JsonDataList} = mochijson2:decode(ResultJson),
  Result = dd_util:to_integer(get_json_value(<<"result">>, JsonDataList)),
  if
    Result =/= -1  -> success;
    true ->
      ?FILE_LOG_ERROR("query_cdkey_call_back error", []),
      throw({custom, "HintInvalidCDKEY"})
  end.

get_json_value(Key, PropList) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      throw({custom, "error json key" ++ dd_util:to_list(Key)});
    Value -> Value
  end.

%%options 是传入参数，不做任何修改直接传出
http_post_req(Url, Pattern, PostData, CallbackFunc, Options) when is_list(Url) andalso is_list(Pattern) andalso is_function(CallbackFunc) ->
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

decode_cdkey_json(CDKeyJson) ->
  #cdkey_rd{
  code_id = dd_util:to_list(get_json_value(<<"id">>, CDKeyJson)),
  code_type = dd_util:to_integer(get_json_value(<<"code_type">>, CDKeyJson)),
  plat_type = dd_util:to_list(get_json_value(<<"plat">>, CDKeyJson)),
  auth_type = dd_util:to_integer(get_json_value(<<"auth">>, CDKeyJson)),
  code_content = dd_util:to_list(get_json_value(<<"content">>, CDKeyJson)),
  status = dd_util:to_integer(get_json_value(<<"status">>, CDKeyJson)),
  expiration_ts = dd_util:to_integer(get_json_value(<<"expiration">>, CDKeyJson))
}.


