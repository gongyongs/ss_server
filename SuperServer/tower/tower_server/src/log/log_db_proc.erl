-module(log_db_proc).
-include("../../deps/file_log/include/file_log.hrl").
-include("../../deps/mysql/include/mysql.hrl").
-include("../cache/cache_def.hrl").
-include("log.hrl").
-include("../csv.hrl").

-export([
  execute/2
]).


execute(DbName, {stat_tollgate_drain,[Index,Len]}) ->
  ?FILE_LOG_DEBUG("log_db_proc => stat_tollgate_drain",[]),
  Sql = "select uin, stage, last_login_ts from account limit " ++ dd_util:to_list(Index) ++ ", " ++ dd_util:to_list(Len) ++ ";",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = Rows}} ->
      Result = lists:map(
        fun([Uin, Stage,LastLoginTs]) ->
          {dd_util:to_integer(Uin), database_util:decode_stage(Stage),dd_util:to_integer(LastLoginTs)}
        end, Rows),
      stat_tollgate_drain(Result);
    Other ->
      ?FILE_LOG_ERROR("stat_tollgate_drain error, reason = ~p", [Other]),
      fail
  end;

execute(DbName, {stat_tower_equip,{Index, Len}}) when Index >= 0 ->
  ?FILE_LOG_DEBUG("log_db_proc => stat_tower_equip",[]),
  Sql = "select hero from account limit " ++ dd_util:to_list(Index) ++ ", " ++ dd_util:to_list(Len) ++ ";",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = Rows}} ->
      HeroList =
        lists:map(                                     %%所有玩家的塔数据    %%n个玩家对应12*n组数据
          fun([HeroData]) ->                          %%最终的列表
            database_util:decode_hero(HeroData)
          end, Rows),
      {success, HeroList};
    Other ->
      ?FILE_LOG_ERROR("query user stage info error, reason = ~p", [Other]),
      fail
  end;

execute(DbName, {stat_power, {Index, Len}}) ->
  if
    Index < 0 ->
      ?FILE_LOG_ERROR("get_user_pay_and_power index error", []),
      fail;
    Len =< 0 ->
      ?FILE_LOG_DEBUG("get_user_pay_and_power length error", []),
      fail;
    true ->
      Sql = "select uin, hero, stage from account limit " ++ dd_util:to_list(Index) ++ ", " ++ dd_util:to_list(Len) ++ ";",
      ?FILE_LOG_DEBUG("get_user_pay_and_power: sql = ~p", [Sql]),
      case mysql:fetch(DbName, Sql) of
        {data, #mysql_result{rows = Rows}} ->
          Result = lists:map(
            fun([Uin, Hero, Stage]) ->
              {dd_util:to_integer(Uin), database_util:decode_hero(Hero), database_util:decode_stage(Stage)}
            end, Rows),
          {success, Result};
        Other ->
          ?FILE_LOG_ERROR("query user stage info error, reason = ~p", [Other]),
          fail
      end
  end.





stat_tollgate_drain(ResultList) ->
  StatList = lists:map(
    fun({Uin, Stage, LastLoginTs}) ->
      BaseTollgateList = Stage#stage.base_tollgate_list,
      TollgateIdList =lists:map(fun(Tmp) -> Tmp#tollgate.id end, BaseTollgateList),
      LgstTollgate =
        case TollgateIdList of
          []-> 0;
          _ -> lists:max(TollgateIdList)
        end,
        ?FILE_LOG_DEBUG("LgstTollgate is ~p",[LgstTollgate]),
        {Uin, LgstTollgate, LastLoginTs}
      end, ResultList),
    {success, StatList}.







