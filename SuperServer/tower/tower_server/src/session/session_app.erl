-module(session_app).
-author('erlangonrails@gmail.com').
-behaviour(application).
-include("../../deps/file_log/include/file_log.hrl").
-include("session.hrl").

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
  ok = dd_util:ensure_app_started(hash_service),
    ok = dd_util:ensure_app_started(mnesia),

    wait_for_tables(),

    {ok, Pid} = session_sup:start_link(),
    log_running_applications(),
    ?FILE_LOG_INFO("session ~s is started in the node ~p", [?SESSION_VERSION, node()]),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

wait_for_tables() ->
  mnesia:wait_for_tables([global_config], infinity).

prep_stop(State) ->
    State.

stop(_State) ->
    dd_ctl:delete_pid_file(),
    ?FILE_LOG_INFO("session ~s is stopped in the node ~p", [?SESSION_VERSION, node()]),
    ok.
    
-spec log_running_applications() ->
    ok.
log_running_applications() ->
    lists:foreach(
      fun(App) ->
          ?FILE_LOG_INFO("running application#~p", [App])
      end, application:which_applications()).

