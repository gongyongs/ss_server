%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十月 2014 下午4:24
%%%-------------------------------------------------------------------
-module(stat_new_player_mission).
-author("zqlt").
-include("../log.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([
  do_stat/1
]).

-record(mission_stat_item,{
  id::string(),
  name::string(),
  start::integer(),
  finish::integer()
}).

-record(mission_user_stat,{
  uin::integer(),
  mission_list::[#mission_stat_item{}]
}).

-record(mission_stat,{
  date::string(),
  mission_list::[#mission_stat_item{}]
}).

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
%%return: list
get_mission_info_list(_, _, []) -> [];
get_mission_info_list(TableList, CurDate, TodayRegisterUinList) ->
  MissionEtsID = ets:new(mission_info, [set, protected, {keypos, #mission_user_stat.uin}]),
  case log_statistics_util:get_select_table(TableList, CurDate) of
    [] -> [];
    List ->
        lists:map(
          fun(TableName) ->
            Sql = lists:flatten(["select Uin, MissionID, MissionDesc, MissionStatus from ",TableName," where EventTime > '", dd_util:time_format_without_hms(CurDate),"' and EventTime < '", dd_util:time_format_without_hms(dd_util:get_next_day(CurDate)), "';"]),
            ?FILE_LOG_DEBUG("query uin backup info : sql = ~p", [Sql]),
            case log_work:execute(query_log, Sql) of
              {success, Rows} ->
                lists:map(
                  fun([UinJson,IDJson, NameJson, StatusMission]) ->
                    Uin = dd_util:to_integer(UinJson),
                    ID = dd_util:to_list(IDJson),
                    Name = dd_util:to_list(NameJson),
                    Status = dd_util:to_integer(StatusMission),
                    {Start, Finish} =
                      case Status of
                        1 -> {1, 0};
                        2 -> {0, 1};
                        _ -> {0, 0}
                      end,
                    MissionStatItem = #mission_stat_item{id = ID, name = Name, start = Start, finish = Finish},
                    case ets:lookup(MissionEtsID, Uin) of
                      [] ->
                        U = #mission_user_stat{uin = Uin, mission_list = [MissionStatItem]},
                        ets:insert(MissionEtsID, U);
                      [UserInfo] ->
                        NU = UserInfo#mission_user_stat{mission_list = [MissionStatItem | UserInfo#mission_user_stat.mission_list]},
                        ets:insert(MissionEtsID, NU)
                    end
                  end, Rows);
              fail ->
                ?FILE_LOG_DEBUG("query uin info error, sql = ~p", [Sql])
            end
          end, List),
      MissionList = lists:foldr(
        fun(Uid, TmpList) ->
          case ets:lookup(MissionEtsID, Uid) of
            [] -> TmpList;
            [UserInfo] ->
              lists:flatten([TmpList, UserInfo#mission_user_stat.mission_list])
          end
        end, [], TodayRegisterUinList),
      Tree =
        lists:foldr(
          fun(Item, TmpTree) ->
            case gb_trees:lookup(Item#mission_stat_item.id, TmpTree) of
              none -> gb_trees:insert(Item#mission_stat_item.id, Item, TmpTree);
              {value, OldItem} -> gb_trees:update(Item#mission_stat_item.id, OldItem#mission_stat_item{start = Item#mission_stat_item.start + OldItem#mission_stat_item.start,
                  finish = Item#mission_stat_item.finish + OldItem#mission_stat_item.finish}, TmpTree)
            end
          end, gb_trees:empty(), MissionList),

      lists:map(fun({_, Item}) -> Item end, gb_trees:to_list(Tree))
  end.

output_Log(MissionData, Interval) when is_record(MissionData, mission_stat) ->
  FileName = log_statistics_util:get_output_filename("mission", Interval),
  {ok, F} = file:open(FileName, write),
  io:format(F, "~s\t~s\t~s\t~s~n", ["MissionID", "MissionName", "MissionStarCount", "MissionFinishCount"]),
  file:close(F),
  lists:map(
    fun(Item) ->
      Out = Item#mission_stat_item.id ++ "\t" ++ Item#mission_stat_item.name ++ "\t" ++ dd_util:to_list(Item#mission_stat_item.start) ++ "\t" ++ dd_util:to_list(Item#mission_stat_item.finish) ++ "\n",
      file:write_file(FileName, unicode:characters_to_binary(list_to_binary(Out)), [append])
      %%io:format(F, "~s\t~ts\t~p\t~p~n", [Item#mission_stat_item.id, unicode:characters_to_list(list_to_binary(Item#mission_stat_item.name)), Item#mission_stat_item.start, Item#mission_stat_item.finish])
    end, MissionData#mission_stat.mission_list).
  %%io:format(F, "~p\t~p\t~s", [PayData#pay_user.pay_user_count, PayData#pay_user.total_pay_times, PayData#pay_user.total_pay_val]),


do_stat({Type, Cycle}) ->
  if
    Type =/= day -> throw({custom, "Type Error"});
    true -> ok
  end,

  {success, RegisterTable} = log_work:execute(get_table_info, "Register"),
  {success, MissionTableInfo} = log_work:execute(get_table_info, "MissionFlow"),

  RegisterTableList = [RegisterTable#log_table.cur_table_name | RegisterTable#log_table.history_table_name],
  MissionTableList = [MissionTableInfo#log_table.cur_table_name | MissionTableInfo#log_table.history_table_name],
  {{CY, CM, CD}, _CurTime} = calendar:local_time(),
  StatDate = dd_util:get_last_day({CY, CM, CD}),

  {TodayRegisterUinList, _TodayRegisterCount} = get_uin_list(RegisterTableList, StatDate),

  MissionData = #mission_stat{date = dd_util:time_format_without_hms(StatDate), mission_list = []},
  List = get_mission_info_list(MissionTableList, StatDate, TodayRegisterUinList),
  NP = MissionData#mission_stat{mission_list =  List},
  ?FILE_LOG_DEBUG("mission info => ~p", [NP]),
  output_Log(NP, {Type, Cycle}),
  {success, dd_util:timestamp()}.