-module(dispatch_app).
-author('erlangonrails@gmail.com').
-behaviour(application).
-include("file_log.hrl").
-include("dispatch.hrl").

-export([start/2, 
         stop/1,
         prep_stop/1]).

-spec start(Type :: term(), StartArgs :: term()) -> 
    {ok, pid()} | {error, 'badarg'}.
start(normal, _StartArgs) ->
	io:format("now in app --->~n",[]),
    dd_ctl:write_pid_file(),
    ok = dd_util:ensure_app_started(sasl),
    ok = dd_util:ensure_app_started(crypto),
    ok = dd_util:ensure_app_started(file_log),
    ok = dd_util:ensure_app_started(mnesia),
    ok = dd_util:ensure_app_started(public_key),
    ok = dd_util:ensure_app_started(hash_service),
    ok = dd_util:ensure_app_started(ssl),
    ok = dd_util:ensure_app_started(inets),
io:format("now in app2 --->~n",[]),
  wait_mnesia_table(),
io:format("now in app3 --->~n",[]),
    {ok, _} = dd_config:start_link(os:getenv("DISPATCH_CFG_PATH")),
    {ok, Pid} = dispatch_sup:start_link(),
io:format("now in app4 --->~n",[]),
  master:notify_master_node_up(),

    log_running_applications(),
	io:format("now in app5 --->~n",[]),
    ?FILE_LOG_INFO("dispatch ~s is started in the node ~p", [?DISPATCH_VERSION, node()]),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

wait_mnesia_table() ->
  mnesia:wait_for_tables([global_config], infinity).

prep_stop(State) ->
    State.

stop(_State) ->
    dd_ctl:delete_pid_file(),
     master:notify_master_node_down(),
    ?FILE_LOG_INFO("dispatch ~s is stopped in the node ~p", [?DISPATCH_VERSION, node()]),
    ok.
    
-spec log_running_applications() ->
    ok.
log_running_applications() ->
    lists:foreach(
      fun(App) ->
          ?FILE_LOG_INFO("running application#~p", [App])
      end, application:which_applications()).

