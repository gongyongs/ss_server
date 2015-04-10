%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2014 17:19
%%%-------------------------------------------------------------------
-module(adminserver_init).
-author("Administrator").

%% API
-export([sysinit/0]).

sysinit() ->
  Node = dd_ctl:get_master_node(),
  mnesia_init(Node),
  init:stop().

mnesia_init(Node) when is_list(Node) ->
  mnesia_init(list_to_atom(Node));

mnesia_init(Node) when is_atom(Node) ->
  io:format("~n==> start the [slave] init mode~n", []),
  application:start(mnesia, permanent),
  mnesia:change_config(extra_db_nodes, [Node]),
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  io:format("==> complete the [slave] init mode~n", []),
  KnownTabs = mnesia:system_info(tables),
  case {lists:member(global_config, KnownTabs)} of
    {true} ->
      io:format("~n==> copy global_config.~n", []),
      case mnesia:add_table_copy(global_config, node(), ram_copies) of
        {atomic, ok} ->
          io:format("~n==> copy global_config success.~n", []);
        {aborted, Reason} ->
          io:format("~n==> copy global_config fail reason=~p.~n", [Reason])
      end;
    _ ->
      io:format("==> Error# can't find all tables in the master node!~n", [])
  end,
  case {lists:member(cache, KnownTabs)} of
    {true} ->
      io:format("~n==> copy cache.~n", []),
      case mnesia:add_table_copy(cache, node(), ram_copies) of
        {atomic, ok} ->
          io:format("~n==> copy cache success.~n", []);
        {aborted, Reason1} ->
          io:format("~n==> copy cache fail reason=~p.~n", [Reason1])
      end;
    _ ->
      io:format("==> Error# can't find all tables in the master node!~n", [])
  end,
  io:format("==> complete the [slave] init mode~n", []),
  ok.