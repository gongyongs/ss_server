-module(gateway_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {success, {ListenIp, ListenPort}} = dd_config:get_cfg(listen_addr),
  Http = web_specs(gateway_http, ListenIp, ListenPort),

  GatewayMonitor =
    {
       gateway_monitor,
       {gateway_monitor, start_link, []},
      permanent,
      5000,
      worker,
      [gateway_monitor]
    },
  Concurrency =
    {concurrency_test,
      {concurrency_test, start_link, []},
      permanent,
      5000,
      worker,
      [concurrency_test]},
  {
    ok,
    {
      {one_for_one, 10, 10},
      [
        GatewayMonitor,
        Http,
        Concurrency
      ]
    }
  }.


web_specs(Mod, ListenIp, Port) ->
  WebConfig = [{ip, ListenIp},
    {port, Port},
    {docroot, gateway_deps:local_path(["priv", "www"])}],
  {Mod,
    {Mod, start, [WebConfig]},
    permanent, 5000, worker, dynamic}.
