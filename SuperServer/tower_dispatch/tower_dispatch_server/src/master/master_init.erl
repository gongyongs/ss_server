%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 七月 2014 下午5:29
%%%-------------------------------------------------------------------
-module(master_init).
-author("zqlt").
-include("../dd_ms.hrl").
%% API
-export([sysinit/0]).

sysinit() ->
  mnesia_init(undefined),
  init:stop().

mnesia_init(undefined) ->
  io:format("~n==> start the [master] init mode~n", []),
  mnesia:create_schema([node()]),
  application:start(mnesia, permanent),


  %%创建登陆的表
  io:format("~n==> create login~n", []),
  case mnesia:create_table(
    login,
    [
      {record_name, login},
      {ram_copies, [node()]},
      {attributes, record_info(fields, login)}]) of
    {atomic, ok} -> io:format("~n==> create login success~n", []);
    {aborted, Reason1} -> io:format("~n==> create login fail reason[~p]~n", [Reason1])
  end,

  %%创建网关的表
  io:format("~n==> create dispatch~n", []),
  case mnesia:create_table(
    dispatch,
    [
      {record_name, dispatch},
      {ram_copies, [node()]},
      {attributes, record_info(fields, dispatch)}]) of
    {atomic, ok} -> io:format("~n==> create dispatch success~n", []);
    {aborted, Reason2} -> io:format("~n==> create login dispatch reason[~p]~n", [Reason2])
  end,

  %%创建全局配置表
  io:format("==> create global_config~n", []),
  case mnesia:create_table(
    global_config,
    [
      {record_name, global_config},
      {ram_copies, [node()]},
      {attributes, record_info(fields, global_config)}]) of
    {atomic, ok} -> io:format("~n==> create global_config success~n", []);
    {aborted, Reason3} -> io:format("~n==> create global_config fail reason[~p]~n", [Reason3])
  end,

  io:format("==> complete the [master] init mode~n", []),
  ok.

