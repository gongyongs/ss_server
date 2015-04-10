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


-export([write_login/1, get_all_login/0, del_login/1]).
-export([write_dispatch/1,get_all_dispatch/0, del_dispatch/1]).

-export([write_config/2, read_config/1]).



write_login(Login) when is_record(Login, login) ->
  case catch mnesia:dirty_write(login, Login) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("write_dispatch fail reason=~p", [Reason]),
      {fail, Reason};
    ok ->
      success
  end.

get_all_login() ->
  case catch mnesia:dirty_select(login, [{#login{_='_'}, [], ['$_']}]) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("dirty_select fail reason=~p", [Reason]),
      fail;
    Logins -> {success, Logins}
  end.

del_login(Node) when is_atom(Node) ->
  case catch mnesia:dirty_delete(login, Node) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("del_login fail reason=~p", [Reason]),
      {fail, Reason};
    ok ->
      success
  end.

write_dispatch(Dispatch) when is_record(Dispatch, dispatch) ->
  case catch mnesia:dirty_write(dispatch, Dispatch) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("write_dispatch fail reason=~p", [Reason]),
      {fail, Reason};
    ok ->
      success
  end.

get_all_dispatch() ->
  case catch mnesia:dirty_select(dispatch, [{#dispatch{_='_'}, [], ['$_']}]) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("dirty_select fail reason=~p", [Reason]),
      fail;
    Dispatchs -> {success, Dispatchs}
  end.

del_dispatch(Node) when is_atom(Node) ->
  case catch mnesia:dirty_delete(dispatch, Node) of
    {'EXIT', Reason} ->
      ?FILE_LOG_ERROR("del_dispatch fail reason=~p", [Reason]),
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
