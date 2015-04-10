-module(token_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {success, {ListenIP, ListenPort}} = dd_config:get_cfg(listen_addr),
  Http = web_specs(token_http,ListenIP, ListenPort),
  TokenCache =
    {token_cache,
      {token_cache, start_link, []},
      permanent,
      5000,
      worker,
      [token_cache]
    },
  GameServerRequest =
    {
      game_server_request,
      {game_server_request, start_link, []},
      permanent,
      5000,
      worker,
      [game_server_request]
    },
   GameServerCache =
     {
       game_server_cache,
       {game_server_cache, start_link, []},
       permanent,
       5000,
       worker,
       [game_server_cache]
     },
  {ok, {{one_for_one, 100, 5},
    [
      Http,
      TokenCache,
      GameServerCache,
      GameServerRequest
    ]
  }}.



web_specs(Mod, ListenIp, Port) ->
  WebConfig = [{ip, ListenIp},
    {port, Port},
    {docroot, token_deps:local_path(["priv", "www"])}],
  {Mod,
    {Mod, start, [WebConfig]},
    permanent, 5000, worker, dynamic}.