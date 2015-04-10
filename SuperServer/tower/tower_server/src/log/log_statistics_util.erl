%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十月 2014 下午5:11
%%%-------------------------------------------------------------------
-module(log_statistics_util).
-author("zqlt").

-include("../../deps/file_log/include/file_log.hrl").
-include("../dd_ms.hrl").
%% API
-export([
  get_select_table/2,
  get_output_filename/2,
  filter_linear_element/2,
  get_cache_node/1
]).

check_dir(Path, DirName) when is_list(DirName) andalso is_list(Path) ->
  case dd_util:make_dir(Path, DirName) of
    {success, _} -> ok;
    {fail, Error} ->
      ?FILE_LOG_ERROR("check dir error: ~p", [Error]),
      throw({custom, "CheckDirError"})
  end.

get_output_filename(Name, {Type, Count}) when is_atom(Type) andalso is_integer(Count) ->
  Path = os:getenv("STAT_LOG_PATH"),
  case Type of
    minute ->
      {D, T} = calendar:local_time(),
      {ND, NT} = dd_util:get_last_few_minute({D, T}, Count),
      NamePre = dd_util:log_time_format({ND, NT}),
      DirName = dd_util:log_time_format_without_hms(ND),
      check_dir(Path, dd_util:to_list(Name)),
      RootPath = Path ++ dd_util:to_list(Name) ++ "/",
      check_dir(RootPath, DirName),
      RootPath ++ DirName ++ "/" ++ NamePre ++ ".log";
    hour ->
      {D, T} = calendar:local_time(),
      {ND, NT} = dd_util:get_last_few_minute({D, T}, Count),
      NamePre = dd_util:log_time_format({ND, NT}),
      DirName = dd_util:log_time_format_without_hms(ND),
      check_dir(Path, dd_util:to_list(Name)),
      RootPath = Path ++ dd_util:to_list(Name) ++ "/",
      check_dir(RootPath, DirName),
      RootPath ++ DirName ++ "/" ++ NamePre ++ ".log";
    _ ->
      {{Y, M, D}, _T} = calendar:local_time(),
      ND = dd_util:get_last_day({Y, M, D}),
      NamePre = dd_util:log_time_format_without_hms(ND),
      check_dir(Path, dd_util:to_list(Name)),
      RootPath = Path ++ dd_util:to_list(Name) ++ "/",
      RootPath ++ NamePre ++ ".log"
  end.

get_select_table(TableList, {GY, GM, GD}) ->
  NTableList = get_sort_table(TableList),
  filter_select_table(NTableList, {GY, GM, GD}).

get_sort_table(TableList) ->
  TableDateList =
    lists:map(
      fun(Name) ->
        {_ , D} = log_util:decode_log_table_name(Name),
        {Name, D}
      end, TableList),
  F = fun({_, LD}, {_, RD}) ->
    case dd_util:time_compare_by_datetime({LD, {0,0,0}}, {RD, {0,0,0}}) of
      1 -> true;
      _ -> false
    end
  end,
  lists:sort(F, TableDateList).


filter_select_table(SortTableList, Date) ->
  filter_select_table_1(SortTableList, Date, []).
filter_select_table_1([], _, OutList) -> OutList;
filter_select_table_1([{Name, HD}], Date, OutList) ->
  case dd_util:time_compare_by_datetime({Date, {0,0,0}}, {HD, {0,0,0}}) of
    1 -> [Name | OutList];
    0 -> [Name | OutList];
    _ -> OutList
  end;
filter_select_table_1([{Name1, HD1}, {Name2, HD2} | T], Date, OutList) ->
  case dd_util:time_compare_by_datetime({Date, {0,0,0}}, {HD1, {0,0,0}}) of
    1 -> [Name1 | OutList];
    0 -> [Name1, Name2 | OutList];
    _ ->
      case dd_util:time_compare_by_datetime({Date, {0,0,0}}, {HD2, {0,0,0}}) of
        1 -> [Name1, Name2 | OutList];
        _ ->
          filter_select_table_1([{Name2, HD2} | T], Date, OutList)
      end
  end.


filter_linear_element(ListA, ListB) ->
  filter_linear_element_1(ListA, ListB, []).
filter_linear_element_1([], [], OutList) -> OutList;
filter_linear_element_1([], _, OutList) -> OutList;
filter_linear_element_1(_, [], OutList) -> OutList;
filter_linear_element_1([L | LT], [R | RT], OutList) ->
  if
    L =:= R -> filter_linear_element_1(LT, RT, [L | OutList]);
    L > R -> filter_linear_element_1([L | LT], RT, OutList);
    L < R -> filter_linear_element_1(LT, [R | RT], OutList)
  end.

get_cache_node(Index) when is_integer(Index)->
  {success, CacheHashRule} = dd_ms:read_config(cache_hash_rule),
  case hash_service_util:find_key_store_node(dd_util:to_list(Index), CacheHashRule) of
    fail ->
      ?FILE_LOG_ERROR("no available data node, ~p", [Index]),
      throw({custom, "HintSystemError"});
    {success, Node} when is_atom(Node) -> {success, Node}
  end.