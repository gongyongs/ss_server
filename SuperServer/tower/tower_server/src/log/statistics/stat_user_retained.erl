%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 十月 2014 下午7:26
%%%-------------------------------------------------------------------
-module(stat_user_retained).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").

%% API
-export([
  do_stat/1
]).

-record(user_remained,{
  date::string(),
  download_count::integer(),
  register_count::integer(),
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

output_Log(RemainedData, Interval) when is_record(RemainedData, user_remained) ->
  FileName = log_statistics_util:get_output_filename("user_retained", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s\t~s\t~s\t~s\t~s\t~s\t~s\t~s~n", ["DownLoadCount", "RegisterUserCount", "UserOneDayRemained", "UserTwoDayRemained","UserThreeDayRemained",
    "UserFourDayRemained","UserFiveDayRemained","UserSixDayRemained","UserSevenDayRemained"]),
  io:format(F, "~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p", [RemainedData#user_remained.download_count, RemainedData#user_remained.register_count, RemainedData#user_remained.one_day_remained, RemainedData#user_remained.two_day_remained,
    RemainedData#user_remained.three_day_remained, RemainedData#user_remained.four_day_remained, RemainedData#user_remained.five_day_remained, RemainedData#user_remained.six_day_remained, RemainedData#user_remained.seven_day_remained]),
  file:close(F).

do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,

  {success, RegisterTable} = log_work:execute(get_table_info, "Register"),
  {success, LoginTable} = log_work:execute(get_table_info, "Login"),
  RegisterTableList = [RegisterTable#log_table.cur_table_name | RegisterTable#log_table.history_table_name],
  LoginTableList = [LoginTable#log_table.cur_table_name | LoginTable#log_table.history_table_name],
  {{CY, CM, CD}, _CurTime} = calendar:local_time(),
  StatDate = dd_util:get_last_day({CY, CM, CD}),
  RemainedData = #user_remained{date = dd_util:time_format_without_hms(StatDate), download_count = 0, register_count = 0, one_day_remained = 0,
    two_day_remained = 0, three_day_remained = 0, four_day_remained = 0, five_day_remained = 0, six_day_remained = 0, seven_day_remained = 0},

  {_TodayRegisterUinList, TodayRegisterCount} = get_uin_list(RegisterTableList, StatDate),
  {TodayLoginUinList, _TodayLoginCount} = get_uin_list(LoginTableList, StatDate),

  {OneDayRegisterUinList, _} = get_uin_list(RegisterTableList, dd_util:get_last_day(StatDate)),
  {TwoDayRegisterUinList, _} = get_uin_list(RegisterTableList, dd_util:get_last_few_day(StatDate, 2)),
  {ThreeDayRegisterUinList, _} = get_uin_list(RegisterTableList, dd_util:get_last_few_day(StatDate, 3)),
  {FourDayRegisterUinList, _} = get_uin_list(RegisterTableList, dd_util:get_last_few_day(StatDate, 4)),
  {FiveDayRegisterUinList, _} = get_uin_list(RegisterTableList, dd_util:get_last_few_day(StatDate, 5)),
  {SixDayRegisterUinList, _} = get_uin_list(RegisterTableList, dd_util:get_last_few_day(StatDate, 6)),
  {SevenDayRegisterUinList, _} = get_uin_list(RegisterTableList, dd_util:get_last_few_day(StatDate, 7)),
  OneRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(OneDayRegisterUinList)),
  TwoRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(TwoDayRegisterUinList)),
  ThreeRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(ThreeDayRegisterUinList)),
  FourRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(FourDayRegisterUinList)),
  FiveRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(FiveDayRegisterUinList)),
  SixRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(SixDayRegisterUinList)),
  SevenRemainedL = log_statistics_util:filter_linear_element(lists:usort(TodayLoginUinList), lists:usort(SevenDayRegisterUinList)),

  NR = RemainedData#user_remained{register_count = TodayRegisterCount, one_day_remained = length(OneRemainedL), two_day_remained = length(TwoRemainedL),
    three_day_remained = length(ThreeRemainedL), four_day_remained = length(FourRemainedL), five_day_remained = length(FiveRemainedL), six_day_remained = length(SixRemainedL), seven_day_remained = length(SevenRemainedL)},
  ?FILE_LOG_DEBUG("remained datat = ~p, one = ~p, two =~p, three = ~p, four = ~p, five = ~p, six = ~p, seven = ~p", [NR, OneDayRegisterUinList, TwoDayRegisterUinList, ThreeDayRegisterUinList, FourDayRegisterUinList, FiveDayRegisterUinList, SixDayRegisterUinList, SevenDayRegisterUinList]),
  output_Log(NR, {Type, Cycle}),
  {success, dd_util:timestamp()}.











