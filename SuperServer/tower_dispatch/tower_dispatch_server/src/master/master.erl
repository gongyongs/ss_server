-module(master).
-author('erlangonrails@gmail.com').
-export([start/0, stop/0]).

-export(
[notify_master_node_up/0,
notify_master_node_down/0]).


-spec start() -> ok | {error, term()}.
start() ->
    application:start(master).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(master).


notify_master_node_up() ->
    Node = list_to_atom(dd_ctl:get_master_node()),
    rpc:cast(Node, master_monitor, node_up, [node()]).

notify_master_node_down() ->
  Node = list_to_atom(dd_ctl:get_master_node()),
  rpc:cast(Node, master_monitor, node_down, [node()]).