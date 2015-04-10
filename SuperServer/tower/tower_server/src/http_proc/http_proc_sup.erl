-module(http_proc_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {success, PlayformMods} = dd_ms:read_config(platform_mod),
  io:format("now in start 5 ~p~n", [PlayformMods]),
%%	Modules = [mod_360, mod_360_ms],
  ModSpecs =
    lists:map(
      fun(Module) ->
        {Module,
          {Module, start_link, []},
          permanent,
          5000,
          worker,
          [Module]}
      end, PlayformMods),

  HttpLog =
    {
      http_proc_log,
      {http_proc_log, start_link, []},
      permanent,
      5000,
      worker,
      [http_proc_log]
    },

  {success, {ListenIp, ListenPort}} = dd_config:get_cfg(listen_addr),
  io:format("now in start 6 ~p~n", [ListenIp]),
  Http = web_specs(http_proc_web, ListenIp, ListenPort),
	io:format("now in start 7 ~p~n", [Http]),
  {ok,
    {
      {one_for_one, 10, 10},
      [HttpLog]
    }
  }.

web_specs(Mod, ListenIp, Port) ->
  WebConfig = [{ip, ListenIp},
    {port, Port},
    {docroot, http_proc_deps:local_path(["priv", "www"])}],
  {Mod,
    {Mod, start, [WebConfig]},
    permanent, 5000, worker, dynamic}.
