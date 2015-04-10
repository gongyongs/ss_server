%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十月 2014 下午2:29
%%%-------------------------------------------------------------------
-module(stat_lottery).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([
  do_stat/1
]).

-record(lottery_stat,{
  date::string(),
  single_lottery_count::integer(),
  single_total_cost::integer(),
  ten_lottery_count::integer(),
  ten_lottery_cost::integer()
}).

%%return: list
get_lottery_info(TableList, CurDate) ->
  LotteryStat = #lottery_stat{ten_lottery_cost = 0, single_lottery_count = 0, single_total_cost = 0, ten_lottery_count = 0, date = dd_util:time_format_without_hms(CurDate)},
  case log_statistics_util:get_select_table(TableList, CurDate) of
    [] -> LotteryStat;
    List ->
        lists:foldr(
          fun(TableName,TmpStat) ->
            Sql = lists:flatten(["select LotteryType, CostCount from ",TableName," where EventTime > '", dd_util:time_format_without_hms(CurDate),"' and EventTime < '", dd_util:time_format_without_hms(dd_util:get_next_day(CurDate)), "';"]),
            ?FILE_LOG_DEBUG("query uin backup info : sql = ~p", [Sql]),
            case log_work:execute(query_log, Sql) of
              {success, Rows} ->
                lists:foldr(
                  fun([Type, Cost], TStat) ->
                    LotteryType = dd_util:to_integer(Type),
                    LotteryCost = dd_util:to_integer(Cost),
                    case LotteryType of
                      1 ->
                        TStat#lottery_stat{single_lottery_count = TStat#lottery_stat.single_lottery_count + 1, single_total_cost = TStat#lottery_stat.single_total_cost + LotteryCost};
                      2 ->
                        TStat#lottery_stat{single_lottery_count = TStat#lottery_stat.ten_lottery_count + 1, single_total_cost = TStat#lottery_stat.ten_lottery_cost + LotteryCost};
                      _ -> TStat
                    end
                  end, TmpStat, Rows);
              fail ->
                ?FILE_LOG_DEBUG("query uin info error, sql = ~p", [Sql]),
                TmpStat
            end
          end, LotteryStat, List)
  end.

output_Log(LotteryStat, Interval) when is_record(LotteryStat, lottery_stat) ->
  FileName = log_statistics_util:get_output_filename("lottery", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s\t~s\t~s~n", ["SingleLottery", "SingleLotteryTotalCost", "TenLottery", "TenLotteryTotalCost"]),
  io:format(F, "~p\t~p\t~p\t~p", [LotteryStat#lottery_stat.single_lottery_count, LotteryStat#lottery_stat.single_total_cost, LotteryStat#lottery_stat.ten_lottery_count, LotteryStat#lottery_stat.ten_lottery_cost]),
  %%io:format(F, "~p\t~p\t~s", [PayData#pay_user.pay_user_count, PayData#pay_user.total_pay_times, PayData#pay_user.total_pay_val]),
  file:close(F).

do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,

  {success, LotteryTableInfo} = log_work:execute(get_table_info, "Lottery"),
  LotteryTableList = [LotteryTableInfo#log_table.cur_table_name | LotteryTableInfo#log_table.history_table_name],
  {{CY, CM, CD}, _CurTime} = calendar:local_time(),
  StatDate = dd_util:get_last_day({CY, CM, CD}),
  LotteryStat = get_lottery_info(LotteryTableList, StatDate),
  ?FILE_LOG_DEBUG("lottery info => ~p", [LotteryStat]),
  output_Log(LotteryStat, {Type, Cycle}),
  {success, dd_util:timestamp()}.
