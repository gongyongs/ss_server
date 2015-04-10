-module(session_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SessionWork =
      {
        session_work,
        {session_work, start_link, []},
        permanent,
        5000,
        worker,
        [session_work]},
    SessionStatistics =
      {
        session_statistics,
        {session_statistics, start_link, []},
        permanent,
        5000,
        worker,
        [session_statistics]
      },
    {ok, {{one_for_one, 100, 5}, [SessionWork, SessionStatistics]}}.
