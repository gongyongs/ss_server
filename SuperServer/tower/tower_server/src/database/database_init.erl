%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 七月 2014 下午1:19
%%%-------------------------------------------------------------------
-module(database_init).
-author("zqlt").

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
  ok.
