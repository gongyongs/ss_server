-module(ranking_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([hash_uin_to_proc/1]).

-define(PROC_SIZE, 20).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
%%   WorkList =
%%     lists:map(
%%       fun(Index) ->
%%         ProcName = get_proc_name(Index),
%%         {
%%           ProcName,
%%           {ranking_work, start_link, [ProcName]},
%%           permanent,
%%           5000,
%%           worker,
%%           [ProcName]
%%         }
%%       end, lists:seq(0, ?PROC_SIZE - 1)),
  RankTollgate =
    {
      ranking_tollgate,
      {ranking_tollgate, start_link, []},
      permanent,
      5000,
      worker,
      [ranking_tollgate]
    },
  RankServer =
    {
      ranking_endless,
      {ranking_endless, start_link, []},
      permanent,
      5000,
      worker,
      [ranking_endless]
    },
    {ok, {{one_for_one, 1000, 500}, [RankServer, RankTollgate]}}.




hash_uin_to_proc(Uin) when is_integer(Uin) ->
  RemV = Uin rem ?PROC_SIZE,
  get_proc_name(RemV).

get_proc_name(Index) when is_integer(Index) ->
  list_to_atom("ranking_work_" ++ dd_util:to_list(Index)).
