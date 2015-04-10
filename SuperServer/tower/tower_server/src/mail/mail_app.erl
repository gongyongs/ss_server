-module(mail_app).
-author('erlangonrails@gmail.com').
-behaviour(application).
-include("../../deps/file_log/include/file_log.hrl").
-include("mail.hrl").

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

    wait_mnesia_table(),

  {ok, _} = dd_config:start_link(os:getenv("MAIL_CFG_PATH")),
    {ok, Pid} = mail_sup:start_link(),

    log_running_applications(),
    ?FILE_LOG_INFO("adminserver ~s is started in the node ~p", [?MAIL_VERSION, node()]),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

wait_mnesia_table() ->
  mnesia:wait_for_tables([global_config], infinity).

stop(_State) ->
    dd_ctl:delete_pid_file(),
    ?FILE_LOG_INFO("adminserver ~s is stopped in the node ~p", [?MAIL_VERSION, node()]),
    ok.
    
-spec log_running_applications() ->
    ok.
log_running_applications() ->
    lists:foreach(
      fun(App) ->
          ?FILE_LOG_INFO("running application#~p", [App])
      end, application:which_applications()).

