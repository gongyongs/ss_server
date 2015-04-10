-module(http_proc_app).
-author('erlangonrails@gmail.com').
-behaviour(application).
-include("../../deps/file_log/include/file_log.hrl").
-include("http_proc.hrl").

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
  ok = dd_util:ensure_app_started(public_key),
  ok = dd_util:ensure_app_started(ssl),
  ok = dd_util:ensure_app_started(inets),
  io:format("now in start 1 ~n", []),
  wait_mnesia_table(),
  io:format("now in start 2 ~n", []),
  {ok, _} = dd_config:start_link(os:getenv("HTTP_PROC_CFG_PATH")),
io:format("now in start 3 ~n", []),
    {ok, Pid} = http_proc_sup:start_link(),
	 io:format("now in start 4 ~n", []),
    log_running_applications(),
    ?FILE_LOG_INFO("http_proc ~s is started in the node ~p", [?HTTP_PROC_VERSION, node()]),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

wait_mnesia_table() ->
  mnesia:wait_for_tables([global_config], infinity).

prep_stop(State) ->
    State.

stop(_State) ->
    dd_ctl:delete_pid_file(),
    ?FILE_LOG_INFO("http_proc ~s is stopped in the node ~p", [?HTTP_PROC_VERSION, node()]),
    ok.
    
-spec log_running_applications() ->
    ok.
log_running_applications() ->
    lists:foreach(
      fun(App) ->
          ?FILE_LOG_INFO("running application#~p", [App])
      end, application:which_applications()).

