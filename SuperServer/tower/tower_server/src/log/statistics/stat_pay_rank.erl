%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十月 2014 下午5:53
%%%-------------------------------------------------------------------
-module(stat_pay_rank).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").


-record(pay_rank_item,{
  uin::integer(),
  dis_name::string(),
  atk::integer(),
  pay_val::float(),
  pay_count::integer()
}).
%% API
-export([
  do_stat/1
]).

%%必须在进程中调用
read_data() ->
  read_data_1(5).

read_data_1(0) -> fail;
read_data_1(Count) ->
  case read_data_to_list(0, ?DB_SINGLE_MAX_READ, []) of
    {success, DataList} ->
      {success, merge(DataList)};
    fail ->
      timer:sleep(60*1000),
      read_data_1(Count - 1)
  end.

read_data_to_list(Index, Len, OutList) ->
  {success, Node} = log_statistics_util:get_cache_node(1),
  case rpc:call(Node, cache, stat_pay_rank, [Index, Len]) of
    {success, DataList} ->
      PayDataList =
        lists:map(
          fun({Uin, DisName, Atk, PayVal, PayNum}) ->
            #pay_rank_item{uin = Uin, dis_name = DisName, atk = Atk, pay_val = PayVal, pay_count = PayNum}
          end, DataList),
      SortFunc =
        fun(#pay_rank_item{pay_val = LV}, #pay_rank_item{pay_val = RV}) ->
          if
            LV > RV -> true;
            true -> false
          end
        end,
      SortAfterList = lists:sort(SortFunc, PayDataList),
      LL = length(SortAfterList),
      if
        LL < Len -> {success, [SortAfterList | OutList]};
        true -> read_data_to_list(Index + Len, Len, [SortAfterList | OutList])
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
merge_1([#pay_rank_item{pay_val = LV} = L | LT], [#pay_rank_item{pay_val = RV} = R | RT], OutList) when LV > RV -> merge_1(LT, [R |RT], [L | OutList]);
merge_1([L | LT], [R | RT], OutList) -> merge_1([L | LT], RT, [R | OutList]).

output_Log(List, Interval) when is_list(List) ->
  FileName = log_statistics_util:get_output_filename("pay_rank", Interval),
  {ok, F} = file:open(FileName, write),
  io:format("~s\t~s\t~s\t~s\t~s~n", ["UserID", "UserName", "UserAtk", "UserTotalPayAmount", "UserTotalPayTimes"]),
  lists:foreach(
    fun(Item) ->
      io:format(F, "~p\t~s\t~p\t~p\t~p~n", [Item#pay_rank_item.uin, Item#pay_rank_item.dis_name, Item#pay_rank_item.atk, Item#pay_rank_item.pay_val, Item#pay_rank_item.pay_count])
    end, List),
  file:close(F).

do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,

  case read_data() of
    {success, DataList} ->
      ?FILE_LOG_DEBUG("read_data success", []),
      output_Log(DataList, {Type, Cycle}),
      {success, dd_util:timestamp()};
    fail ->
      ?FILE_LOG_DEBUG("read_data fail", []),
      fail
  end.
