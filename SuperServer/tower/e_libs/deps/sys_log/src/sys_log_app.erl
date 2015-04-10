-module(sys_log_app).
-author('erlangonrails@gmail.com').
-behaviour(application).
-include("sys_log.hrl").

-export([start/2, 
         stop/1,
         prep_stop/1]).

-spec start(Type :: term(), StartArgs :: term()) -> 
    {ok, pid()} | {error, 'badarg'}.
start(normal, _StartArgs) ->
    dd_ctl:write_pid_file(),
    ok = dd_util:ensure_app_started(sasl),
    ok = dd_util:ensure_app_started(crypto),
    ok = dd_util:ensure_app_started(file_log),
    {ok, Pid} = sys_log_sup:start_link(),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

stop(_State) ->
    dd_ctl:delete_pid_file(),
    ok.
    


