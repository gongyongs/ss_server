%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十月 2014 上午10:36
%%%-------------------------------------------------------------------
-module(stat_endless_tollgate).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([
  do_stat/1
]).

-record(endless_count,{
  date::string(),
  partake_user_count::integer(),
  endless_average_count::integer()
}).

%%return: {UserAccount, average_count}
get_endless_average_count(TableList, CurDate) ->
  case log_statistics_util:get_select_table(TableList, CurDate) of
    [] -> {0, 0};
    List ->
      {UinList, {TotalEndlessCount, TotalEndlessNum}} =
        lists:foldr(
          fun(TableName, {TmpList, {TotalCount, TotalNum}}) ->
            Sql = lists:flatten(["select Uin, EndlessCount from ",TableName," where EventTime > '", dd_util:time_format_without_hms(CurDate),"' and EventTime < '",
              dd_util:time_format_without_hms(dd_util:get_next_day(CurDate)), "' and TollgateType = 2;"]),
            ?FILE_LOG_DEBUG("query endless info : sql = ~p", [Sql]),
            case log_work:execute(query_log, Sql) of
              {success, Rows} ->
                {UinL, {TotalC, TotalN}} = lists:foldr(
                  fun([UinJson, EndlessCount], {TL, {TC, TN}}) ->
                    {[dd_util:to_integer(UinJson) | TL], {TC + dd_util:to_integer(EndlessCount), TN + 1}}
                  end, {[], {0, 0}}, Rows),
                {lists:flatten(TmpList, UinL), {TotalC + TotalCount, TotalN + TotalNum}};
              fail ->
                ?FILE_LOG_DEBUG("query uin info error, sql = ~p", [Sql]),
                {TmpList, {TotalCount, TotalNum}}
            end
          end, {[], {0, 0}}, List),
      StatL = lists:usort(UinList),
      if
        TotalEndlessNum =:= 0 -> {length(StatL), 0};
        true -> {length(StatL), TotalEndlessCount div TotalEndlessNum}
      end
  end.

output_Log(EndlessData, Interval) when is_record(EndlessData, endless_count) ->
  FileName = log_statistics_util:get_output_filename("endless_tollgate", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s~n", ["EndlessParkTakerUser", "EndlessPassAverageCount"]),
  io:format(F, "~p\t~p", [EndlessData#endless_count.partake_user_count, EndlessData#endless_count.endless_average_count]),
  file:close(F).

do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,

  {success, GameEndTableInfo} = log_work:execute(get_table_info, "GameEndFlow"),
  GameEndTableList = [GameEndTableInfo#log_table.cur_table_name | GameEndTableInfo#log_table.history_table_name],
  {{CY, CM, CD}, _CurTime} = calendar:local_time(),
  StatDate = dd_util:get_last_day({CY, CM, CD}),
  EndlessData = #endless_count{date = dd_util:time_format_without_hms(StatDate), endless_average_count = 0, partake_user_count = 0},
  {PartakeUserCount, EndlessCount} = get_endless_average_count(GameEndTableList, StatDate),
  NP = EndlessData#endless_count{endless_average_count = EndlessCount, partake_user_count = PartakeUserCount},
  ?FILE_LOG_DEBUG("endless info => ~p", [NP]),
  output_Log(NP, {Type, Cycle}),
  {success, dd_util:timestamp()}.