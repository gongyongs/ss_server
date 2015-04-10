-module(sys_log).
-author('erlangonrails@gmail.com').
-export([start/0, stop/0]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(sys_log).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(sys_log).
