%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十月 2014 下午3:35
%%%-------------------------------------------------------------------
-module(stat_shop).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([
  do_stat/1
]).

-record(shop_stat_item,{
  item_id::string(),
  count::integer()
}).

-record(shop_stat,{
  date::string(),
  shop_list::[#shop_stat_item{}]
}).

%%return: list
get_consume_stat_list(TableList, CurDate) ->
  case log_statistics_util:get_select_table(TableList, CurDate) of
    [] -> [];
    List ->
      Tree =
        lists:foldr(
          fun(TableName,TmpTree) ->
            Sql = lists:flatten(["select CommodityID from ",TableName," where EventTime > '", dd_util:time_format_without_hms(CurDate),"' and EventTime < '", dd_util:time_format_without_hms(dd_util:get_next_day(CurDate)), "';"]),
            ?FILE_LOG_DEBUG("query uin backup info : sql = ~p", [Sql]),
            case log_work:execute(query_log, Sql) of
              {success, Rows} ->
                lists:foldr(
                  fun([CommodityIDJson], TTree) ->
                    CommodityID = dd_util:to_list(CommodityIDJson),
                    case gb_trees:lookup(CommodityID, TTree) of
                      none -> gb_trees:insert(CommodityID, 1, TTree);
                      {value, Count} -> gb_trees:update(CommodityID, Count + 1, TTree)
                    end
                  end, TmpTree, Rows);
              fail ->
                ?FILE_LOG_DEBUG("query uin info error, sql = ~p", [Sql]),
                TmpTree
            end
          end, gb_trees:empty(), List),
      lists:map(fun({ID, Value}) -> #shop_stat_item{item_id = ID, count = Value} end, gb_trees:to_list(Tree))
  end.

output_Log(ShopStat, Interval) when is_record(ShopStat, shop_stat) ->
  FileName = log_statistics_util:get_output_filename("shop", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s~n", ["CommodityID", "SoldAmount"]),
  lists:map(
    fun(Item) ->
      io:format(F, "~p\t~p~n", [Item#shop_stat_item.item_id, Item#shop_stat_item.count])
    end, ShopStat#shop_stat.shop_list),
  %%io:format(F, "~p\t~p\t~s", [PayData#pay_user.pay_user_count, PayData#pay_user.total_pay_times, PayData#pay_user.total_pay_val]),
  file:close(F).

do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,

  {success, ConsumeTableInfo} = log_work:execute(get_table_info, "ConsumeFlow"),
  ConsumeTableList = [ConsumeTableInfo#log_table.cur_table_name | ConsumeTableInfo#log_table.history_table_name],
  {{CY, CM, CD}, _CurTime} = calendar:local_time(),
  StatDate = dd_util:get_last_day({CY, CM, CD}),
  EndlessData = #shop_stat{date = dd_util:time_format_without_hms(StatDate), shop_list  = []},
  List = get_consume_stat_list(ConsumeTableList, StatDate),
  NP = EndlessData#shop_stat{shop_list = List},
  ?FILE_LOG_DEBUG("tollgate info => ~p", [NP]),
  output_Log(NP, {Type, Cycle}),
  {success, dd_util:timestamp()}.