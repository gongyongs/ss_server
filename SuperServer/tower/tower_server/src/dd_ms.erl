%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 七月 2014 下午4:25
%%%-------------------------------------------------------------------
-module(dd_ms).
-include("dd_ms.hrl").
-include("../deps/file_log/include/file_log.hrl").


-export([write_gateway/1, get_all_gateway/0, del_gateway/1]).
-export([write_cache/1,get_all_cache/0, del_cache/1]).

-export([write_config/2, read_config/1]).



write_gateway(Gateway) when is_record(Gateway, gateway) ->
  case catch mnesia:dirty_write(gateway, Gateway) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("write_gateway fail reason=~p", [Reason]),
      {fail, Reason};
    ok ->
      success
  end.

get_all_gateway() ->
  case catch mnesia:dirty_select(gateway, [{#gateway{_='_'}, [], ['$_']}]) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("dirty_select fail reason=~p", [Reason]),
      fail;
    Gateways -> {success, Gateways}
  end.

del_gateway(Node) when is_atom(Node) ->
  case catch mnesia:dirty_delete(gateway, Node) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("del_gateway fail reason=~p", [Reason]),
      {fail, Reason};
    ok ->
      success
  end.

write_cache(Cache) when is_record(Cache, cache) ->
  case catch mnesia:dirty_write(cache, Cache) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("write_cache fail reason=~p", [Reason]),
      {fail, Reason};
    ok ->
      success
  end.

get_all_cache() ->
  case catch mnesia:dirty_select(cache, [{#cache{_='_'}, [], ['$_']}]) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("dirty_select fail reason=~p", [Reason]),
      fail;
    Caches -> {success, Caches}
  end.

del_cache(Node) when is_atom(Node) ->
  case catch mnesia:dirty_delete(cache, Node) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("del_cache fail reason=~p", [Reason]),
      {fail, Reason};
    ok ->
      success
  end.

%%写入一个配置
write_config(Key, Value) ->
  case catch mnesia:dirty_write(global_config, #global_config{key	= Key, value = Value}) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("write_config fail reason=~p", [Reason]),
      {fail, Reason};
    ok ->
      success
  end.

%%读取一个配置
read_config(Key) ->
  case catch mnesia:dirty_read(global_config, Key) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("read_config fail reason=~p", [Reason]),
      {fail, Reason};
    [] ->
      {fail, no_exists};
    [#global_config{value = Value}] ->
      {success, Value}
  end.
