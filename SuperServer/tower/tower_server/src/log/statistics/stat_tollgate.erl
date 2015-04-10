%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十月 2014 下午1:45
%%%-------------------------------------------------------------------
-module(stat_tollgate).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([
  do_stat/1
]).

-record(tollgate_stat_item,{
  id::integer(),
  start::integer(),
  finish::integer()
}).

-record(tollgate_stat,{
  date::string(),
  tollgate_list::[#tollgate_stat_item{}]
}).

%%return: list
get_tollgate_info_list(TableList, CurDate) ->
  case log_statistics_util:get_select_table(TableList, CurDate) of
    [] -> [];
    List ->
      Tree =
        lists:foldr(
          fun(TableName,TmpTree) ->
            Sql = lists:flatten(["select GameResult, TollgateID, TollgateType from ",TableName," where EventTime > '", dd_util:time_format_without_hms(CurDate),"' and EventTime < '", dd_util:time_format_without_hms(dd_util:get_next_day(CurDate)), "';"]),
            ?FILE_LOG_DEBUG("query uin backup info : sql = ~p", [Sql]),
            case log_work:execute(query_log, Sql) of
              {success, Rows} ->
                lists:foldr(
                  fun([Result, ID, Type], TTree) ->
                    TollgateID = dd_util:to_integer(ID),
                    TollgateType = dd_util:to_integer(Type),
                    NResult = dd_util:to_integer(Result),
                    TollgateResult =
                      if
                        NResult =:= 0 ->  1;
                        true -> 0
                      end,
                    if
                      TollgateType =:= 2 -> TTree;
                      true ->
                        case gb_trees:lookup(TollgateID, TTree) of
                          none -> gb_trees:insert(TollgateID, #tollgate_stat_item{id = TollgateID, start = 1, finish = TollgateResult}, TTree);
                          {value, Item} -> gb_trees:update(TollgateID, Item#tollgate_stat_item{start = Item#tollgate_stat_item.start + 1, finish = Item#tollgate_stat_item.finish + TollgateResult}, TTree)
                        end
                    end
                  end, TmpTree, Rows);
              fail ->
                ?FILE_LOG_DEBUG("query uin info error, sql = ~p", [Sql]),
                TmpTree
            end
          end, gb_trees:empty(), List),
      SortFun = fun(#tollgate_stat_item{id = LID}, #tollgate_stat_item{id = RID}) ->
         if
           LID < RID -> true;
           true -> false
         end
        end,
      StatList = lists:map(fun({_ID, Value}) -> Value end, gb_trees:to_list(Tree)),
      lists:sort(SortFun, StatList)
  end.

output_Log(TollgateData, Interval) when is_record(TollgateData, tollgate_stat) ->
  FileName = log_statistics_util:get_output_filename("tollgate", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s\t~s~n", ["TollgateID", "TollgateStartTimes", "TollgateFinishTimes"]),
  lists:map(
    fun(Item) ->
      io:format(F, "~p\t~p\t~p~n", [Item#tollgate_stat_item.id, Item#tollgate_stat_item.start, Item#tollgate_stat_item.finish])
    end, TollgateData#tollgate_stat.tollgate_list),
  %%io:format(F, "~p\t~p\t~s", [PayData#pay_user.pay_user_count, PayData#pay_user.total_pay_times, PayData#pay_user.total_pay_val]),
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
  EndlessData = #tollgate_stat{date = dd_util:time_format_without_hms(StatDate), tollgate_list = []},
  List = get_tollgate_info_list(GameEndTableList, StatDate),
  NP = EndlessData#tollgate_stat{tollgate_list = List},
  ?FILE_LOG_DEBUG("tollgate info => ~p", [NP]),
  output_Log(NP, {Type, Cycle}),
  {success, dd_util:timestamp()}.
