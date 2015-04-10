-module(adminserver_sup).
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
  Web = web_specs(adminserver_http, ListenIp, ListenPort),
  Cache_proxy =
    {
      adminserver_cache_proxy,
      {adminserver_cache_proxy, start_link, []},
      permanent,
      5000,
      worker,
      [adminserver_cache_proxy]
    },
  Processes = [Web,Cache_proxy],
  Strategy = {one_for_one, 10, 10},
  {ok,
  {Strategy, lists:flatten(Processes)}}.

web_specs(Mod, Ip, Port) ->
  WebConfig = [{ip, Ip},
    {port, Port},
    {docroot, adminserver_deps:local_path(["priv", "www"])}],
{Mod,
{Mod, start, [WebConfig]},
permanent, 5000, worker, dynamic}.


