-module(cache_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([
  init/1,
  hash_uin_to_proc/1,
  hash_uin_to_data/1,
  get_data_proc_list/0
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(PROC_SIZE, 20).
-define(DATA_PROC_SIZE, 10).
%%创建20个gen_server进程处理玩家请求

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  WorkList =
    lists:map(
      fun(Index) ->
        ProcName = get_proc_name(Index),
        {
          ProcName,
          {cache_work, start_link, [ProcName]},
          permanent,
          5000,
          worker,
          [ProcName]
        }
      end, lists:seq(0, ?PROC_SIZE - 1)),

  WorkDataMgr =
        {
          cache_data_mgr,
          {cache_data_mgr, start_link, []},
          permanent,
          5000,
          worker,
          [cache_data_mgr]
        },
  WorkDataList =
    lists:map(
      fun(Index) ->
        ProcName = get_data_proc_name(Index),
        {
          ProcName,
          {cache_data, start_link, [ProcName]},
          permanent,
          5000,
          worker,
          [ProcName]
        }
      end, lists:seq(0, ?DATA_PROC_SIZE - 1)),

  CacheConfigure =
    {
      cache_configure_data,
      {cache_configure_data, start_link, []},
      permanent,
      5000,
      worker,
      [cache_configure_data]
    },
   CacheGuid =
     {
       cache_guid,
       {cache_guid, start_link, []},
       permanent,
       5000,
       worker,
       [cache_guid]
     },
  CacheRanking =
    {
      cache_ranking,
      {cache_ranking, start_link, []},
      permanent,
      5000,
      worker,
      [cache_ranking]
    },

  CacheCsv =
      {cache_csv,
        {cache_csv, start_link, []},
        permanent,
        5000,
        worker,
        [cache_csv]
      },
   CacheLog =
     {
       cache_log,
       {cache_log, start_link, []},
       permanent,
       5000,
       worker,
       [cache_log]
     },
    NewList = lists:merge(WorkList, WorkDataList),
    {ok, {{one_for_one, 1000, 1000},
      [
        WorkDataMgr, CacheConfigure, CacheGuid, CacheRanking, CacheCsv, CacheLog | NewList
      ]
    }}.

hash_uin_to_data(Uin) ->       %%根据玩家ID获取存储表
  RandV = Uin rem ?DATA_PROC_SIZE,
  get_data_proc_name(RandV).

hash_uin_to_proc(Uin) when is_integer(Uin) ->  %%根据玩家ID获取逻辑处理服
   RemV = Uin rem ?PROC_SIZE,     %取余
  get_proc_name(RemV).

get_data_proc_list() ->
  lists:map(
    fun(Index) ->
      get_data_proc_name(Index)
    end, lists:seq(0, ?DATA_PROC_SIZE - 1)).

get_proc_name(Index) when is_integer(Index) ->
  list_to_atom("cache_work_" ++ dd_util:to_list(Index)).

get_data_proc_name(Index) when is_integer(Index)  ->
  list_to_atom("cache_data_" ++ dd_util:to_list(Index)).