-module(log_db_stat_proc).

-include("../../deps/mysql/include/mysql.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-include("log.hrl").
-export([
  execute/2
]).
                                    %%[TableName,[KeyName,[DataList]]
execute(DbName, {write_stat_log, [TableName, Data]})when is_list(Data) ->     %%execute(DbName, {FuncName,[TableName,FuncParam]})
  NData = recompose_type(Data),             %%构建为sql可以多行插入的文件（ ，），（ ，）
  ?FILE_LOG_DEBUG("NData is ~p",[NData]),
  SqlStr = "insert into " ++ dd_util:to_list(TableName) ++ " values" ++ NData ++ ";",
  ?FILE_LOG_DEBUG("write_stat_log: sql_str = ~p", [SqlStr]),
  case mysql:fetch(DbName, SqlStr) of
    {updated, #mysql_result{affectedrows = 1}} -> success;
    {error, {mysql_result, _, _, _, _, _, 1062, _}} ->
      ?FILE_LOG_WARNING("repeat inset sql=~p", [SqlStr]),
      repeat;
    Other ->
      ?FILE_LOG_WARNING("write_log fail other=~p, sql_str =~p", [Other, SqlStr]),
      fail
  end;

execute(_DbName, {user_act_retained, []}) -> success;
execute(DbName, {user_act_retained, DataList}) when is_list(DataList) ->
  ?FILE_LOG_DEBUG("user act retained: ~p", [DataList]),
  InsertSql = insert_user_retained(DataList),
  Len = length(DataList),
  case mysql:fetch(DbName, InsertSql) of
    {updated, #mysql_result{affectedrows = Len}} -> success;
    {updated, #mysql_result{affectedrows = TrueLen}} ->
      ?FILE_LOG_WARNING("insert truely affected len = ~p", [TrueLen]),
      success;
    Other ->
      ?FILE_LOG_WARNING("write_log fail other=~p, sql_str =~p", [Other, InsertSql]),
      fail
  end;

execute(_DbName, {user_atk, []}) -> success;
execute(DbName, {user_atk, DataList}) when is_list(DataList) ->       %%user_atk,DataList
  ?FILE_LOG_DEBUG("user atk = ~p", [DataList]),
  Sql = insert_user_atk(DataList),
  Len = length(DataList),
  case mysql:fetch(DbName, Sql) of
    {updated, #mysql_result{affectedrows = Len}} -> success;
    {updated, #mysql_result{affectedrows = TrueLen}} ->
      ?FILE_LOG_WARNING("insert truely affected len = ~p", [TrueLen]),
      success;
    Other ->
      ?FILE_LOG_WARNING("write_log fail other=~p, sql_str =~p", [Other, Sql]),
      fail
  end;

execute(_DbName, {tower_equip, []}) -> success;
execute(DbName, {tower_equip, DataList}) ->  %%玩家装备
  ?FILE_LOG_DEBUG("tower equip data = ~p", [DataList]),
  InsertSql = insert_tower_equip(DataList),
  Len = length(DataList),
  case mysql:fetch(DbName, InsertSql) of
    {updated, #mysql_result{affectedrows = Len}} -> success;
    {updated, #mysql_result{affectedrows = TrueLen}} ->
      ?FILE_LOG_WARNING("insert truely affected len = ~p", [TrueLen]),
      success;
    Other ->
      ?FILE_LOG_ERROR("insert tower equip data error, reason = ~p", [Other]),
      fail
  end;

execute(_DbName, {tollgate_drain, []}) -> success;
execute(DbName, {tollgate_drain, DataList}) ->
  ?FILE_LOG_DEBUG("tollgate_drain data = ~p", [DataList]),
  InsertSql = insert_tollgate_drain(DataList),
  Len = length(DataList),
  case mysql:fetch(DbName, InsertSql) of
    {updated, #mysql_result{affectedrows = Len}} -> success;
    {updated, #mysql_result{affectedrows = TrueLen}} ->
      ?FILE_LOG_WARNING("insert truely affected len = ~p", [TrueLen]),
      success;
    Other ->
      ?FILE_LOG_ERROR("insert tollgate drain error, reason = ~p", [Other]),
      fail
  end;

execute(_DbName, {material_out, []}) -> success;
execute(DbName, {material_out, DataList}) ->
  ?FILE_LOG_DEBUG("material out data = ~p", [DataList]),
  InsertSql = insert_material_out(DataList),
  Len = length(DataList),
  case mysql:fetch(DbName, InsertSql) of
    {updated, #mysql_result{affectedrows = Len}} -> success;
    {updated, #mysql_result{affectedrows = TrueLen}} ->
      ?FILE_LOG_WARNING("insert truely affected len = ~p", [TrueLen]),
      success;
    Other ->
      ?FILE_LOG_ERROR("insert material out error, reason = ~p", [Other]),
      fail
  end;

execute(_DbName, {endless_tollgate, []}) -> success;
execute(DbName, {endless_tollgate, DataList}) ->
  ?FILE_LOG_DEBUG("endless_tollgate data = ~p", [DataList]),
  InsertSql = insert_endless_tollgate(DataList),
  Len = length(DataList),
  case mysql:fetch(DbName, InsertSql) of
    {updated, #mysql_result{affectedrows = Len}} -> success;
    {updated, #mysql_result{affectedrows = TrueLen}} ->
      ?FILE_LOG_WARNING("insert truely affected len = ~p", [TrueLen]),
      success;
    Other ->
      ?FILE_LOG_ERROR("insert endless_tollgate error, reason = ~p", [Other]),
      fail
  end;

execute(_DbName, {tollgate_stat, []}) -> success;
execute(DbName, {tollgate_stat, DataList}) ->
  ?FILE_LOG_DEBUG("tollgate_stat data = ~p", [DataList]),
  InsertSql = insert_tollgate_stat(DataList),
  Len = length(DataList),
  case mysql:fetch(DbName, InsertSql) of
    {updated, #mysql_result{affectedrows = Len}} -> success;
    {updated, #mysql_result{affectedrows = TrueLen}} ->
      ?FILE_LOG_WARNING("insert truely affected len = ~p", [TrueLen]),
      success;
    Other ->
      ?FILE_LOG_ERROR("insert tollgate_stat error, reason = ~p", [Other]),
      fail
  end;

execute(_DbName, {shop_sell, []}) -> success;
execute(DbName, {shop_sell, DataList}) ->
  ?FILE_LOG_DEBUG("shop_sell data = ~p", [DataList]),
  InsertSql = insert_shop_sell(DataList),
  Len = length(DataList),
  case mysql:fetch(DbName, InsertSql) of
    {updated, #mysql_result{affectedrows = Len}} -> success;
    {updated, #mysql_result{affectedrows = TrueLen}} ->
      ?FILE_LOG_WARNING("insert truely affected len = ~p", [TrueLen]),
      success;
    Other ->
      ?FILE_LOG_ERROR("insert shop_sell error, reason = ~p", [Other]),
      fail
  end.

%%  KeyName = "(Dat,Atk,UserCount)",
%%SqlStr = "insert into StatAtk "++ KeyName ++" values" ++ NData ++ ";",
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insert_tower_equip(DataList) ->
  Sql = "insert into StatTowerEquip (Dat, TowerID, EquipType, Step0Count, Step1Count, Step2Count, Step3Count, Step4Count, Step5Count) values ",
  insert_mass_sql_1(DataList, fun format_tower_equip/1, Sql).

insert_tollgate_drain(DataList) ->
  Sql = "insert into StatTollgateDrain (Dat, TollgateId, DrainCount) values ",
  insert_mass_sql_1(DataList,fun format_tollgate_drain/1, Sql).

insert_material_out(DataList) ->
  Sql = "insert into StatMaterialOutput (Dat, Tool, OutPutCount) values ",
  insert_mass_sql_1(DataList, fun format_material_out/1, Sql).

insert_endless_tollgate(DataList) ->
  Sql = "insert into StatEndlessTollgate (Dat, TollgateId, StartUserCount, PlayTimes, BombTimes, FreezeTimes, TowerCount) values ",
  insert_mass_sql_1(DataList, fun format_endless_tollgate/1, Sql).

insert_user_retained(DataList) ->
  Sql = "insert into StatActivationRetention (Dat, ActUser, RegUser, OneDayRem, TwoDayRem, ThreeDayRem, FourDayRem, FiveDayRem, SixDayRem, SevenDayRem) values ",
  insert_mass_sql_1(DataList, fun format_user_retained/1, Sql).

insert_user_atk(DataList) ->
  Sql = "insert into StatAtk (Dat, Atk, UserCount) values ",
  insert_mass_sql_1(DataList, fun format_user_atk/1, Sql).

insert_tollgate_stat(DataList) ->
  Sql = "insert into StatBaseTollgate (Dat, TollgateID, StartUserCount, PlayTimes, FinishTimes, FinishRate, BombTimes, FreezeTimes, FinishUserCount, TowerCount, OneStarCount, TwoStarCount, ThreeStarCount) values ",
  insert_mass_sql_1(DataList, fun format_tollgate_stat/1, Sql).

insert_shop_sell(DataList) ->
  Sql = "insert into StatShop (Dat, CommodityID, CommodityName, SellCount) values ",
  insert_mass_sql_1(DataList, fun format_shop_sell/1, Sql).

format_shop_sell({DateTime, ID, Name, Count}) ->
  Format =
    [
      "(\"",
      dd_util:to_list(DateTime), "\",\"",
      dd_util:to_list(ID), "\", \"",
      mysql_util:escape(dd_util:to_binary(Name)), "\",",
      dd_util:to_list(Count), ")"
    ],
  lists:flatten(Format).

format_tollgate_stat({DateTime, ID, StartUser, Start, Finish, FinishRate, Bomb, Freeze, FinishUser, TowerCount, One, Two, Three}) ->
  Format =
    [
      "(\"",
      dd_util:to_list(DateTime), "\",",
      dd_util:to_list(ID), ",",
      dd_util:to_list(StartUser), ",",
      dd_util:to_list(Start), ",",
      dd_util:to_list(Finish), ",\"",
      dd_util:to_list(FinishRate), "\",",
      dd_util:to_list(Bomb), ",",
      dd_util:to_list(Freeze), ",",
      dd_util:to_list(FinishUser), ",",
      dd_util:to_list(TowerCount), ",",
      dd_util:to_list(One), ",",
      dd_util:to_list(Two), ",",
      dd_util:to_list(Three), ")"
    ],
  lists:flatten(Format).

format_user_atk({DateTime, AtkType, AtkCount}) ->
  Format =
    [
      "(\"",
      dd_util:to_list(DateTime), "\",\"",
      dd_util:to_list(AtkType), "\",",
      dd_util:to_list(AtkCount), ")"
    ],
  lists:flatten(Format).

format_user_retained({DateTime, Act, Reg, One, Two, Three, Four, Five, Six, Seven}) ->
  Format =
    [
      "(\"",
      dd_util:to_list(DateTime), "\",",
      dd_util:to_list(Act), ",",
      dd_util:to_list(Reg), ",",
      dd_util:to_list(One), ",",
      dd_util:to_list(Two), ",",
      dd_util:to_list(Three), ",",
      dd_util:to_list(Four), ",",
      dd_util:to_list(Five), ",",
      dd_util:to_list(Six), ",",
      dd_util:to_list(Seven), ")"
    ],
  lists:flatten(Format).


format_endless_tollgate({DateTime, TollgateID, StarUser, PlayCount, BombTimes, FreezeTimes, TowerCount}) ->
  Format =
    [
      "(\"",
      dd_util:to_list(DateTime), "\",",
      dd_util:to_list(TollgateID), ",",
      dd_util:to_list(StarUser), ",",
      dd_util:to_list(PlayCount), ",",
      dd_util:to_list(BombTimes), ",",
      dd_util:to_list(FreezeTimes), ",",
      dd_util:to_list(TowerCount), ")"
    ],
  lists:flatten(Format).

format_material_out({DateTime, DropID, Count}) ->
  Format =
    [
      "(\"",
      dd_util:to_list(DateTime), "\",\"",
      dd_util:to_list(DropID), "\",",
      dd_util:to_list(Count), ")"
    ],
  lists:flatten(Format).

format_tollgate_drain({TollgateID, DateTime, Count}) ->
  Format =
    [
      "(\"",
      dd_util:to_list(DateTime), "\",",
      dd_util:to_list(TollgateID), ",",
      dd_util:to_list(Count), ")"
    ],
  lists:flatten(Format).

format_tower_equip({DateTime, TowerID, EquipType, Star0, Star1, Star2, Star3, Star4, Star5}) ->
  Format =
    [
      "(\"",
      dd_util:to_list(DateTime), "\",\"",
      dd_util:to_list(TowerID), "\",",
      dd_util:to_list(EquipType), ",",
      dd_util:to_list(Star0), ",",
      dd_util:to_list(Star1), ",",
      dd_util:to_list(Star2), ",",
      dd_util:to_list(Star3), ",",
      dd_util:to_list(Star4), ",",
      dd_util:to_list(Star5), ")"
    ],
  lists:flatten(Format).

%%组装sql
insert_mass_sql_1([], _FormatFun, Sql) -> Sql ++ ";";
insert_mass_sql_1([Item], FormatFun, Sql) ->
  Sql ++ FormatFun(Item) ++ ";";
insert_mass_sql_1([Item | T], FormatFun, Sql) ->
  insert_mass_sql_1(T, FormatFun, Sql ++ FormatFun(Item) ++ ",").


















































recompose_type(DList) ->
  [H|T] = DList,
  [SubH|SubT] = H,            %%SubH为 string
  HTmp =
    lists:foldl(
      fun(Te,TStr)->
        TStr ++ "," ++ "'" ++ dd_util:to_list(Te) ++"'"
      end,"'"++dd_util:to_list(SubH)++"'",SubT),
  lists:foldl(                                  %%外层，多条数据
    fun(Tmp,DataStr)->
      [TH|TT] = Tmp,                %%TH为 string
      NTH = "'" ++ dd_util:to_list(TH) ++"'",
      NTmp =
        lists:foldl(                            %%内层，单条数据
          fun(Tm,TmpStr)->
            TmpStr ++","++ "'" ++ dd_util:to_list(Tm) ++"'"
          end,NTH,TT),
      DataStr ++ ",(" ++ NTmp ++ ")"
    end,"("++HTmp++")",T).
