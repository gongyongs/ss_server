%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十月 2014 下午6:59
%%%-------------------------------------------------------------------
-module(stat_pay_user_retained).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").

%% API
-export([
  do_stat/1
]).

-record(pay_user_remained,{
  date::string(),
  pay_user_count::integer(),
  one_day_remained::integer(),
  two_day_remained::integer(),
  three_day_remained::integer(),
  four_day_remained::integer(),
  five_day_remained::integer(),
  six_day_remained::integer(),
  seven_day_remained::integer()
}).

%%return: {UinList, Count}
get_uin_list(TableList, CurDate) ->
  case log_statistics_util:get_select_table(TableList, CurDate) of
    [] -> {[], 0};
    List ->
      UinList =
        lists:foldr(
          fun(TableName, TmpList) ->
            Sql = lists:flatten(["select uin from ",TableName," where EventTime > '", dd_util:time_format_without_hms(CurDate),"' and EventTime < '", dd_util:time_format_without_hms(dd_util:get_next_day(CurDate)), "';"]),
            ?FILE_LOG_DEBUG("query uin info : sql = ~p", [Sql]),
            case log_work:execute(query_log, Sql) of
              {success, Rows} ->
                L = lists:map(
                  fun([UinJson]) ->
                    dd_util:to_integer(UinJson)
                  end, Rows),
                [TmpList | L];
              fail ->
                ?FILE_LOG_DEBUG("query uin info error, sql = ~p", [Sql]),
                TmpList
            end
          end, [], List),
      StatL = lists:usort(lists:flatten(UinList)),
      {StatL, length(StatL)}
  end.

output_Log(RemainedData, Interval) when is_record(RemainedData, pay_user_remained) ->
  FileName = log_statistics_util:get_output_filename("pay_user_retained", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s\t~s\t~s\t~s\t~s\t~s\t~s~n", ["PayUserAmount", "PayUserOneDayRemained", "PayUserTwoDayRemained", "PayUserThreeDayRemained", "PayUserFourDayRemained", "PayUserFiveDayRemained",
    "PayUserSixDayRemained", "PayUserSevenDayRemained"]),
  io:format(F, "~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p", [RemainedData#pay_user_remained.pay_user_count, RemainedData#pay_user_remained.one_day_remained, RemainedData#pay_user_remained.two_day_remained,
    RemainedData#pay_user_remained.three_day_remained, RemainedData#pay_user_remained.four_day_remained, RemainedData#pay_user_remained.five_day_remained, RemainedData#pay_user_remained.six_day_remained, RemainedData#pay_user_remained.seven_day_remained]),
  file:close(F).

do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,

  {success, PaySuccess} = log_work:execute(get_table_info, "PaySuccessFlow"),
  {success, LoginTable} = log_work:execute(get_table_info, "Login"),
  PayTableList = [PaySuccess#log_table.cur_table_name | PaySuccess#log_table.history_table_name],
  LoginTableList = [LoginTable#log_table.cur_table_name | LoginTable#log_table.history_table_name],
  {{CY, CM, CD}, _CurTime} = calendar:local_time(),
  StatDate = dd_util:get_last_day({CY, CM, CD}),
  RemainedData = #pay_user_remained{date = dd_util:time_format_without_hms(StatDate), pay_user_count = 0, one_day_remained = 0,
  two_day_remained = 0, three_day_remained = 0, four_day_remained = 0, five_day_remained = 0, six_day_remained = 0, seven_day_remained = 0},

  {TodayLoginUinList, _TodayLoginCount} = get_uin_list(LoginTableList, StatDate),
  {_TodayPayUinList, TodayPayCount} = get_uin_list(PayTableList, StatDate),
  {OneDayPayUinList, _} = get_uin_list(PayTableList, dd_util:get_last_day(StatDate)),
  {TwoDayPayUinList, _} = get_uin_list(PayTableList, dd_util:get_last_few_day(StatDate, 2)),
  {ThreeDayPayUinList, _} = get_uin_list(PayTableList, dd_util:get_last_few_day(StatDate, 3)),
  {FourDayPayUinList, _} = get_uin_list(PayTableList, dd_util:get_last_few_day(StatDate, 4)),
  {FiveDayPayUinList, _} = get_uin_list(PayTableList, dd_util:get_last_few_day(StatDate, 5)),
  {SixDayPayUinList, _} = get_uin_list(PayTableList, dd_util:get_last_few_day(StatDate, 6)),
  {SevenDayPayUinList, _} = get_uin_list(PayTableList, dd_util:get_last_few_day(StatDate, 7)),
  OneRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(OneDayPayUinList)),
  TwoRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(TwoDayPayUinList)),
  ThreeRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(ThreeDayPayUinList)),
  FourRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(FourDayPayUinList)),
  FiveRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(FiveDayPayUinList)),
  SixRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(SixDayPayUinList)),
  SevenRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(SevenDayPayUinList)),

  NR = RemainedData#pay_user_remained{pay_user_count = TodayPayCount, one_day_remained = length(OneRemainedL), two_day_remained = length(TwoRemainedL),
  three_day_remained = length(ThreeRemainedL), four_day_remained = length(FourRemainedL), five_day_remained = length(FiveRemainedL), six_day_remained = length(SixRemainedL), seven_day_remained = length(SevenRemainedL)},
  ?FILE_LOG_DEBUG("pay_user_remained datat = ~p, one = ~p, two =~p, three = ~p, four = ~p, five = ~p, six = ~p, seven = ~p",
    [NR, OneDayPayUinList, TwoDayPayUinList, ThreeDayPayUinList, FourDayPayUinList, FiveDayPayUinList, SixDayPayUinList, SevenDayPayUinList]),
  output_Log(NR, {Type, Cycle}),
  {success, dd_util:timestamp()}.
