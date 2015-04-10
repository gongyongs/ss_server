-module(http_proc).
-author('erlangonrails@gmail.com').
-export([start/0, stop/0]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(http_proc).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(http_proc).
