-module(log_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  LogWork =
    {
      log_work,
      {log_work, start_link, []},
      permanent,
      5000,
      worker,
      [log_work]
    },
  LogStat =
    {
      log_statistics,
      {log_statistics, start_link, []},
      permanent,
      5000,
      worker,
      [log_statistics]
    },
  LogDb=
    {
      log_db,
      {log_db, start_link, []},
      permanent,
      5000,
      worker,
      [log_db]
    },
  LogDbStat=
    {
      log_db_stat,
      {log_db_stat, start_link, []},
      permanent,
      5000,
      worker,
      [log_db_stat]
    },
  LogCsv=
    {
      log_csv,
      {log_csv, start_link, []},
      permanent,
      5000,
      worker,
      [log_csv]
    },
  {ok, {{one_for_one, 10, 10}, [LogWork, LogStat,LogDb,LogDbStat,LogCsv]}}.
