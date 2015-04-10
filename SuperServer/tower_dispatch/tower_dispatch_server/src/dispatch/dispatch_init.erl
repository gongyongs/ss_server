-module(dispatch_init).
-author('erlangonrails@gmail.com').
-export([sysinit/0]).
-include("../dd_ms.hrl").
sysinit() ->
  Node = dd_ctl:get_master_node(),
  io:format("now in dispatch init --->~p~n",[Node]),
  mnesia_init(Node),
  init:stop().

%% ============================================
%% 初始化该Node的mnesia系统, 该函数在安装之后
%% 只需要运行一次.
%%
%% 当该节点是halld cluster中第一个启动的节点时,
%% 参数为undefined; 否则设置为集群中任意一个节点的名字
%% =============================================
-spec mnesia_init(Node :: atom()) -> ok.
mnesia_init(Node) when is_list(Node) ->
  mnesia_init(list_to_atom(Node));
mnesia_init(Node) when is_atom(Node) ->
  io:format("~n==> start the [slave] init mode~n", []),
  A = application:start(mnesia, permanent),
  io:format("~n==> start the [slave] init2 mode~p~n", [A]),
  B = mnesia:change_config(extra_db_nodes, [Node]),
  io:format("~n==> start the [slave] init3 mode~p~n", [B]),
  C = mnesia:change_table_copy_type(schema, node(), disc_copies),
  io:format("~n==> start the [slave] init4 mode~p~n", [C]),
  io:format("==> complete the [slave] init mode~n", []),
  ok.
