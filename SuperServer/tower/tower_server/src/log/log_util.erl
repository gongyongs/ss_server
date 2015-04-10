%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 八月 2014 下午3:13
%%%-------------------------------------------------------------------
-module(log_util).
-author("zqlt").

-include("../../deps/file_log/include/file_log.hrl").
-include("../../deps/mysql/include/mysql.hrl").
-include("log.hrl").

%% API
-export([
  init_table/1,
  update_table_info/2,
  get_table_info/2,
  get_json_value/2,
  switch_table/3
]).

-export([decode_log_table_name/1]).

init_table(DBName) ->
  QuerySql = "select name, history_names, latest_name, last_update_ts, roll_cycle from TableName;",
  case mysql:fetch(DBName, QuerySql) of
    {data, #mysql_result{rows = []}} ->
       lists:foreach(fun(Name) ->
        LogTableItem = create_log_table(DBName, Name),
         insert_table_info(DBName, LogTableItem),
        dd_config:write_cfg(LogTableItem#log_table.name, LogTableItem) end, ?TABLE_LIST);
    {data, #mysql_result{rows = LogTableInfoList}} ->
      LogTableList = decode_table_rd(LogTableInfoList),
      lists:foreach(fun(Item) -> dd_config:write_cfg(Item#log_table.name, Item) end, LogTableList)
  end.

get_table_info(DbName, TableType) when is_list(TableType) ->
  case dd_config:get_cfg(TableType) of
    {success, TableInfo} ->
      {{Year, Month, Day}, _} = dd_util:to_local_time(dd_util:timestamp_to_datetime(TableInfo#log_table.latest_create_ts)),
      {{CurYear, CurMonth, CurDay}, _} = dd_util:to_local_time(dd_util:timestamp_to_datetime(dd_util:timestamp())),
      case check_table_overdue({Year, Month, Day}, {CurYear, CurMonth, CurDay}, TableInfo#log_table.roll_cycle) of
        false -> TableInfo;
        true -> %%已过期
          NTableInfo = create_log_table(DbName, TableType),
          NNT = NTableInfo#log_table{history_table_name = [TableInfo#log_table.cur_table_name | TableInfo#log_table.history_table_name]},
          case update_table_info(DbName, NNT) of
            success ->
              dd_config:write_cfg(NNT#log_table.name, NNT),
              NNT;
            fail ->
              throw({custom, "Update table info error"})
          end
      end;
    fail ->
      TI = create_log_table(DbName, TableType),
      insert_table_info(DbName, TI),
      TI
  end.

switch_table(DbName,TypeName, RollCycle) when is_list(TypeName) andalso RollCycle > 0 ->
  case dd_config:get_cfg(TypeName) of
    {success, TableInfo} ->
      NTableInfo = create_table_with_roll_cycle(DbName, TypeName, RollCycle),
      NNT = NTableInfo#log_table{history_table_name = [TableInfo#log_table.cur_table_name | TableInfo#log_table.history_table_name]},
      case update_table_info(DbName, NNT) of
        success ->
          dd_config:write_cfg(NNT#log_table.name, NNT),
          {success, NNT};
        fail ->
          throw({custom, "Update table info error"})
      end;
    fail ->
      TI = create_table_with_roll_cycle(DbName, TypeName, RollCycle),
      insert_table_info(DbName, TI),
      {success, TI }
  end.

create_log_table(DbName, TypeName) when is_list(TypeName) ->
  case TypeName of
    "MoneyFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?MONEY_FLOW_DB_TABLE_UPDATE_TIME);
    "GameEndFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?GAME_END_DB_TABLE_UPDATE_TIME);
    "Lottery" ->
      create_table_with_roll_cycle(DbName, TypeName, ?LOTTERY_DB_TABLE_UPDATE_TIME);
    "Login" ->
      create_table_with_roll_cycle(DbName, TypeName, ?LOGIN_DB_TABLE_UPDATE_TIME);
    "Register" ->
      create_table_with_roll_cycle(DbName, TypeName, ?REGISTER_DB_TABLE_UPDATE_TIME);
    "RewardFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?REWARD_DB_TABLE_UPDATE_TIME);
    "PaySuccessFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?PAY_DB_TABLE_UPDATE_TIME);
    "PayFailFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?PAY_DB_TABLE_UPDATE_TIME);
    "ConsumeFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?CONSUME_DB_TABLE_UPDATE_TIME);
    "StrengthenFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?STRENGTHEN_DB_TABLE_UPDATE_TIME);
    "OperationTimeFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?OPERATION_TIME_DB_TABLE_UPDATE_TIME);
    "MissionFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?MISSION_DB_TABLE_UPDATE_TIME);
    "AchievementFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?ACHIEVEMENT_DB_TABLE_UPDATE_TIME);
    "CNPaySuccessFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?CN_PAY_TABLE_UPDATE_TIME);
    "CNPayFailFlow" ->
      create_table_with_roll_cycle(DbName, TypeName, ?CN_PAY_TABLE_UPDATE_TIME);
    _Other ->
      throw({custom, "log type error"})
  end.

create_table_with_roll_cycle(DbName, TypeName, RollCycle) when RollCycle > 0 ->
  TableName = create_log_table_name(TypeName),
  case TypeName of
    "MoneyFlow" ->
      CreateSQL = dd_util:to_list(io_lib:format(?MONEY_FLOW_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table MoneyFlow, table name = ~p", [TableName]),
          #log_table{name = "MoneyFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "GameEndFlow" ->
      CreateSQL = dd_util:to_list(io_lib:format(?GAME_END_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table GameEndFlow, table name = ~p", [TableName]),
          #log_table{name = "GameEndFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "Lottery" ->
      CreateSQL = dd_util:to_list(io_lib:format(?LOTTERY_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table Lottery, table name = ~p", [TableName]),
          #log_table{name = "Lottery", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "Login" ->
      CreateSQL = dd_util:to_list(io_lib:format(?LOGIN_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table Login, table name = ~p", [TableName]),
          #log_table{name = "Login", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "Register" ->
      CreateSQL = dd_util:to_list(io_lib:format(?REGISTER_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table Register, table name = ~p", [TableName]),
          #log_table{name = "Register", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "RewardFlow" ->
      CreateSQL = dd_util:to_list(io_lib:format(?REWARD_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table RewardFlow, table name = ~p", [TableName]),
          #log_table{name = "RewardFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "PaySuccessFlow" ->
      CreateSQL = dd_util:to_list(io_lib:format(?PAY_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table PaySuccessFlow, table name = ~p", [TableName]),
          #log_table{name = "PaySuccessFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "PayFailFlow" ->
      CreateSQL = dd_util:to_list(io_lib:format(?PAY_FAIL_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table PayFailFlow, table name = ~p", [TableName]),
          #log_table{name = "PayFailFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "ConsumeFlow" ->
      CreateSQL = dd_util:to_list(io_lib:format(?CONSUME_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table ConsumeFlow, table name = ~p", [TableName]),
          #log_table{name = "ConsumeFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "StrengthenFlow" ->
      CreateSQL = dd_util:to_list(io_lib:format(?STRENGTHEN_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table StrengthenFlow, table name = ~p", [TableName]),
          #log_table{name = "StrengthenFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "OperationTimeFlow" ->
      CreateSQL = dd_util:to_list(io_lib:format(?OPERATION_TIME_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table OperationTimeFlow, table name = ~p", [TableName]),
          #log_table{name = "OperationTimeFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "MissionFlow" ->
      CreateSQL = dd_util:to_list(io_lib:format(?MISSION_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table MissionFlow, table name = ~p", [TableName]),
          #log_table{name = "MissionFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "AchievementFlow" ->
      CreateSQL = dd_util:to_list(io_lib:format(?ACHIEVEMENT_CREATE_SQL, [TableName])),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table AchievementFlow, table name = ~p", [TableName]),
          #log_table{name = "AchievementFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "CNPaySuccessFlow" ->
      CreateSQL = dd_util:to_list((io_lib:format(?CNPAY_SQL, [TableName]))),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table CNPaySuccessFlow, table name = ~p", [TableName]),
          #log_table{name = "CNPaySuccessFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    "CNPayFailFlow" ->
      CreateSQL = dd_util:to_list((io_lib:format(?CNPAY_SQL, [TableName]))),
      case mysql:fetch(DbName, CreateSQL) of
        {updated, #mysql_result{affectedrows = 0}} ->
          ?FILE_LOG_INFO("create table CNPayFailFlow, table name = ~p", [TableName]),
          #log_table{name = "CNPayFailFlow", history_table_name = [], latest_create_ts = dd_util:timestamp(), cur_table_name = TableName, roll_cycle = RollCycle};
        {error, Reason} -> throw({custom, Reason})
      end;
    _Other ->
      throw({custom, "log type error"})
  end;
create_table_with_roll_cycle(_DbName, _TypeName, _RollCycle) ->
  throw({custom, "rollcycle error"}).


insert_table_info(DBName, LogTable) when is_record(LogTable, log_table) ->
  InsertSql = create_table_info_sql(LogTable),
  case mysql:fetch(DBName, InsertSql) of
    {updated, #mysql_result{affectedrows = 1}} ->
      dd_config:write_cfg(LogTable#log_table.name, LogTable);
    Other ->
      ?FILE_LOG_DEBUG("insert table info error, reason = ~p",[Other]),
      throw({custom, "InsertError"})
  end.

create_table_info_sql(LogTable) when is_record(LogTable, log_table) ->
  mysql_util:insert_query("TableName",
    ["name", "history_names", "latest_name", "last_update_ts", "roll_cycle"],
    [
      encode_name(LogTable#log_table.name),
      encode_history_name(LogTable#log_table.history_table_name),
      encode_latest_name(LogTable#log_table.cur_table_name),
      encode_last_update_ts(LogTable#log_table.latest_create_ts),
      encode_roll_cycle(LogTable#log_table.roll_cycle)
    ]
  ).
update_table_info(DBName, LogTable) when is_record(LogTable, log_table) ->
  UpdateSql = update_table_info_sql(LogTable),
  ?FILE_LOG_DEBUG("update table info sql: ~p", [UpdateSql]),
  case mysql:fetch(DBName, UpdateSql) of
    {updated, #mysql_result{affectedrows = 1}} ->success;
    {updated, #mysql_result{affectedrows = 0}} ->
      ?FILE_LOG_WARNING("update affectedrows = 0", []),
      success;
    Other ->
      ?FILE_LOG_DEBUG("update table info error, reason = ~p", [Other]),
      fail
  end.


update_table_info_sql(LogTable) when is_record(LogTable,log_table) ->
  mysql_util:update_query("TableName",
    ["name", "history_names", "latest_name", "last_update_ts", "roll_cycle"],
    [
      encode_name(LogTable#log_table.name),
      encode_history_name(LogTable#log_table.history_table_name),
      encode_latest_name(LogTable#log_table.cur_table_name),
      encode_last_update_ts(LogTable#log_table.latest_create_ts),
      encode_roll_cycle(LogTable#log_table.roll_cycle)
    ],
    "name='" ++ dd_util:to_list(LogTable#log_table.name)++ "'") .

decode_table_rd(TableInfoList) ->
  lists:map(
    fun([Name, HistoryNames, LatestName, LastUpdateTs, Cycle]) ->
      #log_table{
        name = decode_name(Name),
        history_table_name = decode_history_name(HistoryNames),
        latest_create_ts = decode_last_update_ts(LastUpdateTs),
        cur_table_name = decode_latest_name(LatestName),
        roll_cycle = decode_roll_cycle(Cycle)
      }
    end, TableInfoList).

encode_name(Name) -> dd_util:to_list(Name).
encode_latest_name(Name) -> dd_util:to_list(Name).
encode_last_update_ts(Time) -> dd_util:to_list(Time).
encode_roll_cycle(Cycle) -> dd_util:to_list(Cycle).
encode_history_name(HistoryNameList) ->
  Value = lists_to_str_with_split(HistoryNameList, ";"),
  mysql_util:escape(Value).

decode_name(Name) -> dd_util:to_list(Name).
decode_latest_name(Name) -> dd_util:to_list(Name).
decode_last_update_ts(Time) -> dd_util:to_integer(Time).
decode_roll_cycle(Cycle) -> dd_util:to_integer(Cycle).
decode_history_name(HistoryName) ->
  string:tokens(dd_util:to_list(HistoryName), ";").



get_json_value(Key, PropList) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      throw({custom, "error json key" ++ dd_util:to_list(Key)});
    Value -> Value
  end.


create_log_table_name(TypeName) when is_list(TypeName) ->
  CurTime = dd_util:timestamp(),
  {{Year, Month, Day}, _} = dd_util:to_local_time(dd_util:timestamp_to_datetime(CurTime)),
  TypeName ++ "_" ++ dd_util:to_list(Year) ++ "_" ++ dd_util:to_list(Month) ++ "_" ++ dd_util:to_list(Day).

decode_log_table_name(TableName) when is_list(TableName) ->
  ?FILE_LOG_DEBUG("TableName is ~p",[TableName]),
  [TypeName, Year, Month, Day] = string:tokens(TableName, "_"),
  {dd_util:to_list(TypeName), {dd_util:to_integer(Year), dd_util:to_integer(Month), dd_util:to_integer(Day)}}.

check_table_overdue(_, _, 0) -> false;
check_table_overdue({TableYear, TableMonth, TableDay}, {CurYear, CurMonth, CurDay}, Cycle) when Cycle > 0->
  {CYear, CMonth, CDay} = dd_util:get_last_few_day({CurYear, CurMonth, CurDay}, Cycle),
  case dd_util:time_compare_by_datetime({{TableYear, TableMonth, TableDay}, {0,0,0}}, {{CYear, CMonth, CDay}, {0,0,0}}) of
    -1 -> true;   %%已过期
    _Other -> false
  end.

lists_to_str_with_split(List, Split) when is_list(List) ->
  lists:foldr(
    fun(ID, TmpL) ->
      TmpL ++ dd_util:to_list(ID) ++ dd_util:to_list(Split)
    end, "", List).

