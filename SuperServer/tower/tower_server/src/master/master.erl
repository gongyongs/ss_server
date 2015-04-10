-module(master).
-author('erlangonrails@gmail.com').
-export([start/0, stop/0]).
-export([
notify_node_up/0,
notify_node_down/0
]).

-include("../../deps/file_log/include/file_log.hrl").

-spec start() -> ok | {error, term()}.
start() ->
    application:start(master).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(master).


notify_node_up() ->
  Node = list_to_atom(dd_ctl:get_master_node()),
  rpc:cast(Node, master_monitor, node_up, [node()]).

notify_node_down() ->
  Node = list_to_atom(dd_ctl:get_master_node()),
  ?FILE_LOG_INFO("notify master node down, ~p", [Node]),
  Value = rpc:cast(Node, master_monitor, node_down, [node()]),
  ?FILE_LOG_INFO("notify master node down, result = ~p", [Value]).