-module(mail_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([
  hash_uin_to_proc/1,
  get_random_proc/0
]).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(PROC_SIZE, 10).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  WorkList =
    lists:map(
      fun(Index) ->
        ProcName = get_proc_name(Index),
        {
          ProcName,
          {mail_work, start_link, [ProcName]},
          permanent,
          5000,
          worker,
          [ProcName]
        }
      end, lists:seq(0, ?PROC_SIZE - 1)),
  MailID =
    {
      mail_id,
      {mail_id, start_link, []},
      permanent,
      5000,
      worker,
      [mail_id]
    },
  MailTemplate =
    {
      mail_template,
      {mail_template, start_link, []},
      permanent,
      5000,
      worker,
      [mail_template]
    },
  MailData =
    {
      mail_data,
      {mail_data, start_link, []},
      permanent,
      5000,
      worker,
      [mail_data]
    },
  {ok, {{one_for_one, 100, 10},
    [
      MailID, MailTemplate, MailData | WorkList
    ]
  }}.


get_random_proc() ->
  RandV = dd_util:random_in_range(?PROC_SIZE),
  get_proc_name(RandV).

hash_uin_to_proc(Uin) when is_integer(Uin) ->
  RemV = Uin rem ?PROC_SIZE,
  get_proc_name(RemV).

get_proc_name(Index) when is_integer(Index) ->
  list_to_atom("mail_work_" ++ dd_util:to_list(Index)).