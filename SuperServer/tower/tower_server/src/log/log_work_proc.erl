%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 八月 2014 下午2:17
%%%-------------------------------------------------------------------
-module(log_work_proc).
-author("zqlt").
-include("../../deps/mysql/include/mysql.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-include("log.hrl").
%% API
%% API
-export([execute/2]).

execute(DbName, {write_log, LogData}) ->
  [_, TableType | InsertValue] = string:tokens(LogData, "|"),
  LoginInfo = log_util:get_table_info(DbName, TableType),
  SqlStr = lists:flatten(["insert into ", LoginInfo#log_table.cur_table_name, " values(", InsertValue], ")"),
  ?FILE_LOG_DEBUG("write_log: sql_str = ~p", [SqlStr]),
  case mysql:fetch(DbName, SqlStr) of
    {updated, #mysql_result{affectedrows = 1}} -> success;
    {error, {mysql_result, _, _, _, _, _, 1062, _}} ->
      ?FILE_LOG_WARNING("repeat inset sql=~p", [SqlStr]),
      repeat;
    Other ->
      ?FILE_LOG_WARNING("write_log fail other=~p, sql_str =~p", [Other, SqlStr]),
      fail
  end;
execute(DbName, {switch_table, {TableType, RollCycle}}) ->
  ?FILE_LOG_DEBUG("switch table: type ~p, cycle = ~p", [TableType, RollCycle]),
  if
    RollCycle =< 0 -> throw({custom, "rollcycle error"});
    true ->
      case lists:member(TableType, ?TABLE_LIST) of
        false -> throw({custom, "table type error"});
        true -> ok
      end
  end,
  {success, LoginInfo} = log_util:switch_table(DbName, TableType, RollCycle),
  ?FILE_LOG_DEBUG("table info ~p", [LoginInfo]),
  success;
execute(_DbName, {get_table_info, TableType}) ->
  ?FILE_LOG_DEBUG("get table info, type = ~p", [TableType]),
  case dd_config:get_cfg(TableType) of
    {success, TableInfo} -> {success, TableInfo};
    fail -> fail
  end;
execute(DbName, {query_log, Sql}) ->
  ?FILE_LOG_DEBUG("query_data => sql=~p", [Sql]),
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} -> {success, []};
    {data, #mysql_result{rows = Rows}} -> {success, Rows};
    Other ->
      ?FILE_LOG_DEBUG("query data error, reason = ~p", [Other]),
      fail
  end.




