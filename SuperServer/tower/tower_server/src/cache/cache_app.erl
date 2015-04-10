-module(cache_app).
-author('erlangonrails@gmail.com').
-behaviour(application).
-include("../../deps/file_log/include/file_log.hrl").
-include("cache.hrl").
-include("../dd_ms.hrl").
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
  ok = dd_util:ensure_app_started(inets),
    wait_mnesia_table(),
    {ok, _} = dd_config:start_link(os:getenv("CACHE_CFG_PATH")),

    {ok, Pid} = cache_sup:start_link(),

    master:notify_node_up(),
    dd_ms:write_cache(#cache{node = node(), info = #cache_info{}}),
    log_running_applications(),
    ?FILE_LOG_INFO("cache ~s is started in the node ~p", [?CACHE_VERSION, node()]),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

stop(_State) ->
    dd_ctl:delete_pid_file(),
    master:notify_node_down(),
    ?FILE_LOG_INFO("cache ~s is stopped in the node ~p", [?CACHE_VERSION, node()]),
    ok.


wait_mnesia_table() ->
  mnesia:wait_for_tables([global_config], infinity).

-spec log_running_applications() ->
    ok.
log_running_applications() ->
    lists:foreach(
      fun(App) ->
          ?FILE_LOG_INFO("running application#~p", [App])
      end, application:which_applications()).

