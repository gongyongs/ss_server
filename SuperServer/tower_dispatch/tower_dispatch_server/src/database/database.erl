-module(database).
-author('erlangonrails@gmail.com').
-export([start/0, stop/0]).




-spec start() -> ok | {error, term()}.
start() ->
    application:start(database).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(database).
