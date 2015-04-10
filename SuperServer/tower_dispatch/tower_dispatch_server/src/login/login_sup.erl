-module(login_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).
-define(PROC_SIZE, 10).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
%%  WorkList =
%%    lists:map(
%%      fun(Index) ->
%%        ProcName = get_proc_name_by_index(Index),
%%        {
%%          ProcName,
%%          {login_work, start_link, [ProcName]},
%%          permanent,
%%          5000,
%%          worker,
%%          [ProcName]
%%        }
%%      end, lists:seq(0, ?PROC_SIZE - 1)),
  Work =
    {
      login_work,
      {login_work, start_link, []},
      permanent,
      5000,
      worker,
      [login_work]
    },

    LoginCache =
      {
        login_cache,
        {login_cache, start_link, []},
        permanent,
        5000,
        worker,
        [login_cache]
      },
    {ok, {{one_for_one, 100, 5}, [LoginCache, Work]}}.
