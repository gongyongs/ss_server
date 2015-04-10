%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十月 2014 下午3:02
%%%-------------------------------------------------------------------
-module(stat_strengthen).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([
  do_stat/1
]).

-record(strengthen_stat,{
  date::string(),
  upgrade_user_count::integer(),
  upgrade_total_count::integer(),
  advance_user_count::integer(),
  advance_total_count::integer()
}).

%%return: list
get_strengthen_info_list(TableList, CurDate) ->
  StrengthenData = #strengthen_stat{date = dd_util:log_time_format_without_hms(CurDate), upgrade_total_count = 0,
    upgrade_user_count = 0, advance_total_count = 0, advance_user_count = 0},
  case log_statistics_util:get_select_table(TableList, CurDate) of
    [] -> StrengthenData;
    List ->
      {UpgradeList, AdvancedList} =
      lists:foldr(
        fun(TableName,{TempUpgradeList, TempAdvancedList}) ->
          Sql = lists:flatten(["select Uin, StrengthenType from ",TableName," where EventTime > '", dd_util:time_format_without_hms(CurDate),"' and EventTime < '", dd_util:time_format_without_hms(dd_util:get_next_day(CurDate)), "';"]),
          ?FILE_LOG_DEBUG("query uin backup info : sql = ~p", [Sql]),
          case log_work:execute(query_log, Sql) of
            {success, Rows} ->
              lists:foldr(
                fun([UinJson, Type], {TU, TA}) ->
                  StrengthenType = dd_util:to_integer(Type),
                  Uin = dd_util:to_integer(UinJson),
                  case StrengthenType of
                    0 -> {[Uin | TU], TA};
                    1 -> {TU, [Uin | TA]};
                    _ -> {TU,TA}
                  end
                end, {TempUpgradeList, TempAdvancedList}, Rows);
            fail ->
              ?FILE_LOG_DEBUG("query uin info error, sql = ~p", [Sql]),
              {TempUpgradeList, TempAdvancedList}
          end
        end, {[], []}, List),
      UpgradeUserList = lists:usort(UpgradeList),
      AdvancedUserList = lists:usort(AdvancedList),
      StrengthenData#strengthen_stat{date = dd_util:log_time_format_without_hms(CurDate), upgrade_total_count = length(UpgradeList),
        upgrade_user_count = length(UpgradeUserList), advance_total_count = length(AdvancedList), advance_user_count = length(AdvancedUserList)}
  end.

output_Log(StrengthenStat, Interval) when is_record(StrengthenStat, strengthen_stat) ->
  FileName = log_statistics_util:get_output_filename("strengthen", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s\t~s\t~s", ["UpgradeUserAmount", "UpgradeTotalTimes", "AdvancedUserAmount", "AdvancedTotalTimes"]),
  io:format(F, "~p\t~p\t~p\t~p", [StrengthenStat#strengthen_stat.upgrade_user_count, StrengthenStat#strengthen_stat.upgrade_total_count, StrengthenStat#strengthen_stat.advance_user_count, StrengthenStat#strengthen_stat.advance_total_count]),
  %%io:format(F, "~p\t~p\t~s", [PayData#pay_user.pay_user_count, PayData#pay_user.total_pay_times, PayData#pay_user.total_pay_val]),
  file:close(F).

do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,

  {success, StrengthenTableInfo} = log_work:execute(get_table_info, "StrengthenFlow"),
  StrengthenTableList = [StrengthenTableInfo#log_table.cur_table_name | StrengthenTableInfo#log_table.history_table_name],
  {{CY, CM, CD}, _CurTime} = calendar:local_time(),
  StatDate = dd_util:get_last_day({CY, CM, CD}),
  StrengthenStat = get_strengthen_info_list(StrengthenTableList, StatDate),
  ?FILE_LOG_DEBUG("strengthen info => ~p", [StrengthenStat]),
  output_Log(StrengthenStat, {Type, Cycle}),
  {success, dd_util:timestamp()}.