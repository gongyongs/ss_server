-module(database_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([
  hash_uin_to_proc/1,
  get_proc_name/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(PROC_SIZE, 10).
-define(DB_PROC_SIZE, 5).
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  GuidWorkList =
    lists:map(
      fun(Index) ->
        ProcName = get_proc_name(Index),
        {
          ProcName,
          {database_guid_proc, start_link, [ProcName]},
          permanent,
          5000,
          worker,
          [ProcName]
        }
      end, lists:seq(0, ?PROC_SIZE - 1)),

%  DatabaseMonitor =
%    lists:map(
%      fun(Index) ->
%        ProcName1 = get_db_proc_name(Index),
%        {
%          ProcName1,
%          {database_monitor, start_link, [ProcName1]},
%          permanent,
%          5000,
%          worker,
%          [ProcName1]
%        }
%      end, lists:seq(0, ?DB_PROC_SIZE - 1)),
  DatabaseMonitor =
    {
      database_monitor,
      {database_monitor, start_link, []},
      permanent,
      5000,
      worker,
      [database_monitor]
    },

%%    L = lists:flatten(),

    {ok, {{one_for_one, 100, 100}, [DatabaseMonitor | GuidWorkList]}}.

hash_uin_to_proc(Uin) when is_integer(Uin) ->
  RemV = Uin rem ?PROC_SIZE,
  get_proc_name(RemV).

get_proc_name(Index) ->
  list_to_atom("alloc_guid_" ++ dd_util:to_list(Index)).