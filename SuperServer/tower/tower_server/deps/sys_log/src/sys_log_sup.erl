-module(sys_log_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
  SysLog =
    {sys_log_server,
      {sys_log_server, start_link, []},
      permanent,
      brutal_kill,
      supervisor,
      [sys_log_server]},
  {ok,{{one_for_one, 100, 5},[SysLog]}}.
