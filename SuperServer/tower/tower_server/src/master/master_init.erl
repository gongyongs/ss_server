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
-include("../../deps/hash_service/src/hash_service.hrl").
%% API
-export([sysinit/0]).

sysinit() ->
  mnesia_init(undefined),
  init:stop().

mnesia_init(undefined) ->
  io:format("~n==> start the [master] init mode~n", []),
  mnesia:create_schema([node()]),
  application:start(mnesia, permanent),


  %%创建缓存节点表
  io:format("~n==> create cache~n", []),
  case mnesia:create_table(
    cache,
    [
      {record_name, cache},
      {ram_copies, [node()]},
      {attributes, record_info(fields, cache)}]) of
    {atomic, ok} -> io:format("~n==> create cache success~n", []);
    {aborted, Reason1} -> io:format("~n==> create cache fail reason[~p]~n", [Reason1])
  end,

  %%创建接入网关服务器的表
  io:format("~n==> create gateway~n", []),
  case mnesia:create_table(
    gateway,
    [
      {record_name, gateway},
      {ram_copies, [node()]},
      {attributes, record_info(fields, gateway)}]) of
    {atomic, ok} -> io:format("~n==> create gateway success~n", []);
    {aborted, Reason2} -> io:format("~n==> create gateway reason[~p]~n", [Reason2])
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


