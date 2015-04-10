-module(login_init).
-export([sysinit/0]).
-include("../dd_ms.hrl").
sysinit() ->
  Node = dd_ctl:get_master_node(),
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
  application:start(mnesia, permanent),
  mnesia:change_config(extra_db_nodes, [Node]),
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  io:format("==> complete the [slave] init mode~n", []),
  ok.