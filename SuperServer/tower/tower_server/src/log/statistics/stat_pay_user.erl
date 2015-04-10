%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十月 2014 下午7:22
%%%-------------------------------------------------------------------
-module(stat_pay_user).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([
  do_stat/1
]).

-record(pay_user,{
  date::string(),
  pay_user_count::integer(),
  total_pay_times::integer(),
  total_pay_val::string()
}).

%%return: {PayCount, UserCount, PayVal}
get_pay_info_list(TableList, CurDate) ->
  case log_statistics_util:get_select_table(TableList, CurDate) of
    [] -> {0, 0, "0.0"};
    List ->
      {UinList, TotalPayV} =
        lists:foldr(
          fun(TableName, {TmpList, TmpV}) ->
            Sql = lists:flatten(["select Uin, BuyBackup from ",TableName," where EventTime > '", dd_util:time_format_without_hms(CurDate),"' and EventTime < '", dd_util:time_format_without_hms(dd_util:get_next_day(CurDate)), "';"]),
            ?FILE_LOG_DEBUG("query uin backup info : sql = ~p", [Sql]),
            case log_work:execute(query_log, Sql) of
              {success, Rows} ->
                {UinL, PayV} = lists:foldr(
                  fun([UinJson, PayVal], {TL, TV}) ->
                    {[dd_util:to_integer(UinJson) | TL], dd_util:list_to_float(PayVal, 0.0) + TV}
                  end, {[], 0.0}, Rows),
                {lists:flatten(TmpList, UinL), PayV};
              fail ->
                ?FILE_LOG_DEBUG("query uin info error, sql = ~p", [Sql]),
                {TmpList, TmpV}
            end
          end, {[], 0.0}, List),
      StatL = lists:usort(UinList),
      {length(UinList), length(StatL), dd_util:float_to_string(TotalPayV)}
  end.

output_Log(PayData, Interval) when is_record(PayData, pay_user) ->
  FileName = log_statistics_util:get_output_filename("pay_user", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s\t~s~n", ["PayUserAmount", "TotalPayTimes", "TotalPayValue"]),
  io:format(F, "~p\t~p\t~s", [PayData#pay_user.pay_user_count, PayData#pay_user.total_pay_times, PayData#pay_user.total_pay_val]),
  file:close(F).

do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,

  {success, PaySuccess} = log_work:execute(get_table_info, "PaySuccessFlow"),
  PayTableList = [PaySuccess#log_table.cur_table_name | PaySuccess#log_table.history_table_name],
  {{CY, CM, CD}, _CurTime} = calendar:local_time(),
  StatDate = dd_util:get_last_day({CY, CM, CD}),
  PayData = #pay_user{date = dd_util:time_format_without_hms(StatDate), pay_user_count = 0, total_pay_times = 0, total_pay_val = []},
  {PayCount, UserCount, TotalPay} = get_pay_info_list(PayTableList, StatDate),
  NP = PayData#pay_user{pay_user_count = UserCount, total_pay_times = PayCount, total_pay_val = TotalPay},
  ?FILE_LOG_DEBUG("total pay info => ~p", [NP]),
  output_Log(NP, {Type, Cycle}),
  {success, dd_util:timestamp()}.

