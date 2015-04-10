-module(log).
-author('erlangonrails@gmail.com').
-export([start/0, stop/0]).
-export([
  write_log/1,
  switch_table/2,
  call_write_log/1
]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(log).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(log).


write_log(LogData) when is_list(LogData) ->
  cast_write_log(LogData).

cast_write_log(LogData) when is_list(LogData) ->
  log_work:cast_write_log(LogData).

switch_table(TableType, RollCycle) ->
  log_work:switch_table(TableType, RollCycle).

call_write_log(LogData) when is_list(LogData) ->
  log_work:call_write_log(LogData).



