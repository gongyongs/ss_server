%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十月 2014 下午8:21
%%%-------------------------------------------------------------------
-module(stat_user_data).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").


-record(user_power_item,{
  uin::integer(),
  dis_name::string(),
  atk::integer()
}).

-record(user_data_item,{
  uin::integer(),
  dis_name::string(),
  atk::integer(),
  income_gold::integer(),
  outcome_gold::integer(),
  income_gem::integer(),
  outcome_gem::integer()
}).
%% API
-export([
  do_stat/1
]).

get_user_data(PowerList, TableList, Date) ->
  lists:map(
    fun(PowerItem) ->
      get_one_user_data(PowerItem, TableList, Date)
    end, PowerList).

get_one_user_data(PowerInfo, TableList, Date) ->
  UserData = #user_data_item{uin = PowerInfo#user_power_item.uin, atk = PowerInfo#user_power_item.atk,
    dis_name = PowerInfo#user_power_item.dis_name, income_gem = 0, outcome_gem = 0, income_gold = 0, outcome_gold = 0},
  case log_statistics_util:get_select_table(TableList, Date) of
    [] -> UserData;
    List ->
      lists:foldr(
        fun(TableName,TmpStat) ->
          Sql = lists:flatten(["select MoneyType, MoneyBefore,MoneyAfter from ",TableName," where EventTime > '", dd_util:time_format_without_hms(Date),"' and EventTime < '",
            dd_util:time_format_without_hms(dd_util:get_next_day(Date)), "' and Uin = ", dd_util:to_list(PowerInfo#user_power_item.uin), ";"]),
          ?FILE_LOG_DEBUG("query uin backup info : sql = ~p", [Sql]),
          case log_work:execute(query_log, Sql) of
            {success, Rows} ->
              lists:foldr(
                fun([MoneyTypeJson, Before, After], TStat) ->
                  MoneyType = dd_util:to_integer(MoneyTypeJson),
                  BeforeMoney = dd_util:to_integer(Before),
                  AfterMoney = dd_util:to_integer(After),
                  Dif = AfterMoney - BeforeMoney,
                  {InCome, OutCome} =
                    if
                      Dif > 0 -> {Dif, 0};
                      true -> {0, BeforeMoney - AfterMoney}
                    end,
                  case MoneyType of
                    1 ->
                      UserData#user_data_item{income_gem = UserData#user_data_item.income_gem + InCome, outcome_gem = UserData#user_data_item.outcome_gem + OutCome};
                    2 ->
                      UserData#user_data_item{income_gold = UserData#user_data_item.income_gold + InCome, outcome_gold = UserData#user_data_item.outcome_gold + OutCome};
                    _ -> TStat
                  end
                end, TmpStat, Rows);
            fail ->
              ?FILE_LOG_DEBUG("query uin info error, sql = ~p", [Sql]),
              TmpStat
          end
        end, UserData, List)
  end.

%%必须在进程中调用
read_data(TableList, Date) ->
  read_data_1(5, {TableList, Date}).

read_data_1(0, {_TableList, _Date}) -> fail;
read_data_1(Count, {TableList, Date}) ->
  case read_data_to_list(0, ?DB_SINGLE_MAX_READ, [],  {TableList, Date}) of
    {success, DataList} ->
      {success, merge(DataList)};
    fail ->
      timer:sleep(60*1000),
      read_data_1(Count - 1, {TableList, Date})
  end.

read_data_to_list(Index, Len, OutList, {TableList, Date}) ->
  {success, Node} = log_statistics_util:get_cache_node(1),
  case rpc:call(Node, cache, stat_power_rank, [Index, Len]) of
    {success, DataList} ->
      PowerDataList =
        lists:map(
          fun({Uin, DisName, Atk, _PayVal, _PayNum}) ->
            #user_power_item{uin = Uin, dis_name = DisName, atk = Atk}
          end, DataList),
      UserDataList = get_user_data(PowerDataList, TableList, Date),
      SortFunc =
        fun(#user_data_item{atk = LV}, #user_data_item{atk = RV}) ->
          if
            LV > RV -> true;
            true -> false
          end
        end,
      SortAfterList = lists:sort(SortFunc, UserDataList),
      LL = length(SortAfterList),
      if
        LL < Len -> {success, [SortAfterList | OutList]};
        true -> read_data_to_list(Index + Len, Len, [SortAfterList | OutList], {TableList, Date})
      end;
    fail ->
      ?FILE_LOG_ERROR("stat_rank => read_data_to_list fail", []),
      fail;
    _ -> fail
  end.

%%采用多进程处理，并且保证接收每一个进程的结果是按顺序的
merge(InList) ->
  OutPidList = merge_sort(InList, [], ?MAX_PROCESS_COUNT),
  SortRetList =
    lists:foldr(
      fun(Pid, TmpRetList) ->
        receive
          {Pid, {sort_ret, SortRet}} ->
            [SortRet | TmpRetList]
        after
          5*1000 ->
            %%超时
            ?FILE_LOG_ERROR("stat_rank => sort ret timeout", []),
            TmpRetList
        end
      end, [], OutPidList),
  case SortRetList of
    [] -> throw("stat_rank => sort exception");
    [SortRetItem] -> SortRetItem;
    SortRetList -> merge(SortRetList)
  end.

merge_sort([], OutPidList, _ProcessCount) -> OutPidList;
merge_sort(InList, OutPidList, 0) ->
  merge_sort(InList, OutPidList, ?MAX_PROCESS_COUNT);
merge_sort(InList, OutPidList, ProcessCount) when length(InList) =:= 1 ->
  merge_sort([[] | InList], OutPidList, ProcessCount);
merge_sort([L1, L2 | T], OutPidList, ProcessCount) ->
  Pid = self(),
  RPid = spawn(fun() -> Pid ! {self(), {sort_ret, merge_1(L1, L2, [])}} end),
  merge_sort(T, [RPid | OutPidList], ProcessCount - 1).

merge_1([], [], OutList) -> lists:reverse(OutList);
merge_1([], R, OutList) ->  lists:reverse(OutList) ++ R;
merge_1(L, [], OutList) -> lists:reverse(OutList) ++ L;
merge_1([#user_data_item{atk = LV} = L | LT], [#user_data_item{atk = RV} = R | RT], OutList) when LV > RV -> merge_1(LT, [R |RT], [L | OutList]);
merge_1([L | LT], [R | RT], OutList) -> merge_1([L | LT], RT, [R | OutList]).


output_Log(List, Interval) when is_list(List) ->
  FileName = log_statistics_util:get_output_filename("user_data", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s\t~s\t~s\t~s\t~s\t~s~n", ["UserID", "UserName", "UserAtk", "UserTotalIncomeGold", "UserTotalOutcomeGold", "UserTotalIncomeGem", "UserTotalOutcomeGem"]),
  lists:foreach(
    fun(Item) ->
      io:format(F, "~p\t~s\t~p\t~p\t~p\t~p\t~p~n", [Item#user_data_item.uin, Item#user_data_item.dis_name, Item#user_data_item.atk, Item#user_data_item.income_gold,
        Item#user_data_item.outcome_gold,Item#user_data_item.income_gem, Item#user_data_item.outcome_gem])
    end, List),
  file:close(F).

do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,
  {success, MoneyTableInfo} = log_work:execute(get_table_info, "MoneyFlow"),
  MoneyTableList = [MoneyTableInfo#log_table.cur_table_name | MoneyTableInfo#log_table.history_table_name],
  {{CY, CM, CD}, _CurTime} = calendar:local_time(),
  StatDate = dd_util:get_last_day({CY, CM, CD}),
  {success, DataList} = read_data(MoneyTableList, StatDate),
  output_Log(DataList, {Type, Cycle}),
  {success, dd_util:timestamp()}.
