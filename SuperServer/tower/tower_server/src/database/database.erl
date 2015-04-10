-module(database).
-author('erlangonrails@gmail.com').
-export([start/0, stop/0]).
-export([alloc_guid/1]).



-spec start() -> ok | {error, term()}.
start() ->
    application:start(database).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(database).

alloc_guid(Uin) when is_integer(Uin) ->
  ProcName = database_sup:hash_uin_to_proc(Uin),
  database_guid_proc:get_guid(ProcName).