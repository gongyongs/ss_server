%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十月 2014 下午3:52
%%%-------------------------------------------------------------------
-module(stat_reward).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([
  do_stat/1
]).

-record(reward_stat_item,{
  type::string(),
  stat_list::[],
  user_count::integer(),
  out_gold_count::integer(),
  out_gem_count::integer()
}).

-record(reward_stat,{
  date::string(),
  reward_list::[#reward_stat_item{}]
}).

get_reward_type(Type) when is_integer(Type) ->
  case Type of
    1 -> "LoginReward";
    2 -> "Activity";
    3 -> "Mission";
    4 -> "Achievement";
    _ -> "unknow"
  end.

%%return: list
get_reward_stat_list(TableList, CurDate) ->
  case log_statistics_util:get_select_table(TableList, CurDate) of
    [] -> [];
    List ->
      Tree =
        lists:foldr(
          fun(TableName,TmpTree) ->
            Sql = lists:flatten(["select Uin, RewardType, RewardItemType, RewardItemCount from ",TableName," where EventTime > '", dd_util:time_format_without_hms(CurDate),"' and EventTime < '", dd_util:time_format_without_hms(dd_util:get_next_day(CurDate)), "';"]),
            ?FILE_LOG_DEBUG("query uin backup info : sql = ~p", [Sql]),
            case log_work:execute(query_log, Sql) of
              {success, Rows} ->
                lists:foldr(
                  fun([UinJson, Type, ItemType, ItemCount], TTree) ->
                    Uin = dd_util:to_integer(UinJson),
                    RewardType = get_reward_type(dd_util:to_integer(Type)),
                    RewardItemType = dd_util:to_integer(ItemType),
                    RewardItemCount = dd_util:to_integer(ItemCount),

                    {OutGem, OutGold} =
                      case RewardItemType of
                        1 -> {0, RewardItemCount};
                        2 -> {RewardItemCount, 0};
                        _ -> {0,0}
                      end,

                    case gb_trees:lookup(RewardType, TTree) of
                      none -> gb_trees:insert(RewardType, #reward_stat_item{type = RewardType, stat_list = [Uin], out_gem_count = OutGem, out_gold_count = OutGold}, TTree);
                      {value, Item} -> gb_trees:update(RewardType, Item#reward_stat_item{stat_list = [Uin | Item#reward_stat_item.stat_list],
                        out_gold_count = Item#reward_stat_item.out_gold_count + OutGold, out_gem_count = Item#reward_stat_item.out_gem_count + OutGem}, TTree)
                    end
                  end, TmpTree, Rows);
              fail ->
                ?FILE_LOG_DEBUG("query uin info error, sql = ~p", [Sql]),
                TmpTree
            end
          end, gb_trees:empty(), List),
      lists:map(fun({_ID, Value}) -> Value#reward_stat_item{user_count = length(lists:usort(Value#reward_stat_item.stat_list))} end, gb_trees:to_list(Tree))
  end.

output_Log(RewardStat, Interval) when is_record(RewardStat, reward_stat) ->
  FileName = log_statistics_util:get_output_filename("reward", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s\t~s\t~s~n", ["RewardType", "PartakeUserAmount", "OutComeTotalGold", "OutComeTotalGem"]),
    lists:map(
    fun(Item) ->
      io:format(F, "~s\t~p\t~p\t~p~n", [Item#reward_stat_item.type, Item#reward_stat_item.user_count, Item#reward_stat_item.out_gold_count, Item#reward_stat_item.out_gem_count])
    end, RewardStat#reward_stat.reward_list),
  %%io:format(F, "~p\t~p\t~s", [PayData#pay_user.pay_user_count, PayData#pay_user.total_pay_times, PayData#pay_user.total_pay_val]),
  file:close(F).

do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,

  {success, RewardTableInfo} = log_work:execute(get_table_info, "RewardFlow"),
  RewardTableList = [RewardTableInfo#log_table.cur_table_name | RewardTableInfo#log_table.history_table_name],
  {{CY, CM, CD}, _CurTime} = calendar:local_time(),
  StatDate = dd_util:get_last_day({CY, CM, CD}),
  RewardData = #reward_stat{date = dd_util:time_format_without_hms(StatDate), reward_list = []},
  List = get_reward_stat_list(RewardTableList, StatDate),
  NP = RewardData#reward_stat{reward_list = List},
  ?FILE_LOG_DEBUG("reward info => ~p", [NP]),
  output_Log(NP, {Type, Cycle}),
  {success, dd_util:timestamp()}.