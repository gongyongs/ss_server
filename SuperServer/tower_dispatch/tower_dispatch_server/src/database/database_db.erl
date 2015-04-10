%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 七月 2014 下午3:29
%%%-------------------------------------------------------------------
-module(database_db).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
-include("../../deps/mysql/include/mysql.hrl").
%%处理数据库相关操作
-include("../login/login.hrl").
-include("../dispatch/dispatch.hrl").
-define(TIME_OUT, 10000).
%% API
-export([execute/2]).
-export([
  generate_uin/1,
  result_to_list/1
]).

execute(_DbName, {register,_}) ->
  ok;

execute(DbName, {login_by_uname, {UName, Device}})  ->
  case query_user_info_by_uname(DbName, UName) of
    not_exist->
      case query_user_info_by_device(DbName, Device) of
        not_exist ->  %%不存在
          {success, Uin} = generate_uin(DbName),
          UserInfo = database_util:init_user_info(Device, UName, Uin),
          Sql = database_util:get_create_user_info_sql(UserInfo),
          {updated, #mysql_result{affectedrows = 1}} = mysql:fetch(DbName, Sql),
          {success, UserInfo};
        {success, DeviceUserInfo} ->
          case DeviceUserInfo#user_info.uname of
            "" ->
              NDeviceUserInfo = DeviceUserInfo#user_info{uname = UName, platform_info = DeviceUserInfo#user_info.platform_info#platform{player_id = UName}},
              {success, NDeviceUserInfo};
            _ ->
              {success, Uin} = generate_uin(DbName),
              UserInfo = database_util:init_user_info("", UName, Uin),
              Sql = database_util:get_create_user_info_sql(UserInfo),
              {updated, #mysql_result{affectedrows = 1}} = mysql:fetch(DbName, Sql),
              {success, UserInfo}
          end;
        {fail, Reason} -> {fail, Reason}
      end;
    {success, UNameUserInfo} ->
      case UNameUserInfo#user_info.device of
        "" ->
          case query_user_info_by_device(DbName, Device) of
            not_exist ->
              NUNameUserInfo = UNameUserInfo#user_info{device = Device},
              {success, NUNameUserInfo};
            {success, _} ->  {success, UNameUserInfo};
            {fail, Reason} -> {fail, Reason}
          end;
        _ -> {success, UNameUserInfo}
      end;
    {fail, Reason} -> {fail, Reason}
  end;

execute(DbName, {login_by_uname, UName})  ->
  case query_user_info_by_uname(DbName, UName) of
    not_exist->
      {success, Uin} = generate_uin(DbName),
      UserInfo = database_util:init_user_info("", UName, Uin),
      Sql = database_util:get_create_user_info_sql(UserInfo),
      {updated, #mysql_result{affectedrows = 1}} = mysql:fetch(DbName, Sql),
      {success, UserInfo};
    {success, UserInfo} -> {success, UserInfo};
    {fail, Reason} -> {fail, Reason}
  end;

execute(DbName, {login_by_device, Device}) ->
  case query_user_info_by_device(DbName, Device) of
    not_exist ->
      {success, Uin} = generate_uin(DbName),
      UserInfo = database_util:init_user_info(Device, "", Uin),
      Sql = database_util:get_create_user_info_sql(UserInfo),
      {updated, #mysql_result{affectedrows = 1}} = mysql:fetch(DbName, Sql),
      {success, UserInfo};
    {success, UserInfo} -> {success, UserInfo};
    Other ->
      ?FILE_LOG_DEBUG("create_user_by_uname error => reason = ~p", [Other]),
      {fail, "Create_user_error"}
  end;

execute(DbName, {bond_device, {Plat, Device, PlayerID, DisName, Friend}}) ->
  case query_user_info_by_device(DbName, Device) of
    not_exist -> {fail, "AccountNotExist"};
    {success, DeviceUserInfo} ->
      case query_user_info_by_uname(DbName, PlayerID) of
        not_exist ->
          NUserInfo = DeviceUserInfo#user_info{uname = PlayerID, dis_name = DisName, add_ts = dd_util:timestamp(),
            platform_info = #platform{plat_friend = Friend#friends.friend_list, player_dis_name = DisName,
            plat_type = Plat, player_id = PlayerID}},
          Sql = database_util:update_user_info_sql(NUserInfo),
          {updated, #mysql_result{affectedrows = 1}} = mysql:fetch(DbName, Sql),
          {success, NUserInfo};
        {success, UNameUserInfo} ->
          Uid = DeviceUserInfo#user_info.uin,
          case UNameUserInfo#user_info.uin of
            Uid ->
              NUserInfo = UNameUserInfo#user_info{device = Device, dis_name = DisName, add_ts = dd_util:timestamp(),
                platform_info = UNameUserInfo#user_info.platform_info#platform{plat_friend = Friend#friends.friend_list,
                player_dis_name = DisName}},
              Sql = database_util:update_user_info_sql(NUserInfo),
              {updated, #mysql_result{affectedrows = _}} = mysql:fetch(DbName, Sql),
              {success, NUserInfo};
            _ -> account_has_bond
          end;
        Other ->
          ?FILE_LOG_ERROR("bond device error, error = ~p", [Other]),
          {fail, "HintSystemError"}
      end;
    DOther ->
      ?FILE_LOG_ERROR("bond device error, error = ~p", [DOther]),
      {fail, "SystemError"}
  end;

execute(DbName, {get_user_by_device, Device}) ->
  ?FILE_LOG_DEBUG("DB; get_uin_by_device[~p]", [Device]),
  case query_user_info_by_device(DbName, Device) of
    not_exist-> not_exist;
    {success, UserInfo} -> {success, UserInfo};
    {fail, Reason} -> {fail, Reason}
  end;


execute(DbName, {get_user_by_uname, {UName, _Device}}) ->
    try
      case query_user_info_by_uname(DbName, UName) of
        fail -> {fail, "db error"};
        not_exist -> not_exist;
        {success, User} -> {success, User}
      end
    catch
      What:Type ->
        ?FILE_LOG_ERROR("what=~p, type =~p, stack =~p", [What, Type, erlang:get_stacktrace()]),
        exception
    end;
execute(DbName, {create_user_by_uname, UName}) ->
  case query_user_info_by_uname(DbName, UName) of
    not_exist ->
      {success, Uin} = generate_uin(DbName),
      UserInfo = database_util:init_user_info("", UName, Uin),
      Sql = database_util:get_create_user_info_sql(UserInfo),
      {updated, #mysql_result{affectedrows = 1}} = mysql:fetch(DbName, Sql),
      {success, UserInfo};
    Other ->
      ?FILE_LOG_DEBUG("create_user_by_uname error => reason = ~p", [Other]),
      {fail, "Create_user_error"}
  end;
execute(DbName, {create_user_by_device, Device}) ->
  case query_user_info_by_device(DbName, Device) of
    not_exist ->
      {success, Uin} = generate_uin(DbName),
      UserInfo = database_util:init_user_info(Device, "", Uin),
      Sql = database_util:get_create_user_info_sql(UserInfo),
      {updated, #mysql_result{affectedrows = 1}} = mysql:fetch(DbName, Sql),
      {success, UserInfo};
    Other ->
      ?FILE_LOG_DEBUG("create_user_by_uname error => reason = ~p", [Other]),
      {fail, "Create_user_error"}
  end;
execute(DbName, {update_user_info, UserInfo}) ->
  UpdateSql = database_util:update_user_info_sql(UserInfo),
  ?FILE_LOG_DEBUG("update user info sql: ~p", [UpdateSql]),
  case mysql:fetch(DbName, UpdateSql) of
    {updated, #mysql_result{affectedrows = 1}} ->success;
    {updated, #mysql_result{affectedrows = 0}} ->
      ?FILE_LOG_WARNING("update affectedrows = 0", []),
      success
  end;

execute(DbName, {get_friend_data, Uin}) ->
  case query_user_info_by_uin(DbName, Uin) of
    fail -> {fail, "uin not exist"};
    {success, UserInfo} ->
      case UserInfo#user_info.platform_info#platform.plat_friend of
        [] ->
          {success, []};
        List ->
          {Sql, OutList} = create_query_friend_sql(List),
          case OutList of
            [] -> {success, []};
            [ID] ->
              NSql = "select uin, uname, dis_name from login where uname=" ++ dd_util:to_list(ID),
              case mysql:fetch(DbName, NSql) of
                {data, #mysql_result{rows = Rows}} ->
                  Result = lists:map(
                    fun([Uid, UName, DisName]) ->
                      {dd_util:to_integer(Uid), dd_util:to_list(UName), dd_util:to_list(DisName)}
                    end, Rows),
                  {success, Result};
                Other ->
                  ?FILE_LOG_ERROR("query friend stage info error, reason = ~p", [Other]),
                  {fail, "query data error"}
              end;
            _ ->
              ?FILE_LOG_INFO("query friend data: SQL = ~p", [Sql]),
              case mysql:fetch(DbName, Sql) of
                {data, #mysql_result{rows = Rows}} ->
                  Result = lists:map(
                    fun([Uid, UName, DisName]) ->
                      {dd_util:to_integer(Uid), dd_util:to_list(UName), dd_util:to_list(DisName)}
                    end, Rows),
                  {success, Result};
                Other ->
                  ?FILE_LOG_ERROR("query friend stage info error, reason = ~p", [Other]),
                  {fail, "query data error"}
              end
          end
      end
  end;

execute(DbName, {check_register_user, Uin}) ->
  case query_user_info_by_uin(DbName, Uin) of
    fail -> {fail, "uin not exist"};
    {success, UserInfo} ->
      case UserInfo#user_info.platform_info#platform.plat_friend of
        [] ->
          {success, {UserInfo, []}};
        List ->
          {Sql, OutList} = create_query_friend_sql(List),
          case OutList of
            [] -> {success, {UserInfo, []}};
            [ID] ->
              NSql = "select uin, uname, dis_name from login where uname=" ++ dd_util:to_list(ID),
              case mysql:fetch(DbName, NSql) of
                {data, #mysql_result{rows = Rows}} ->
                  Result = lists:map(
                    fun([Uid, UName, DisName]) ->
                      {dd_util:to_integer(Uid), dd_util:to_list(UName), dd_util:to_list(DisName)}
                    end, Rows),
                  {success, {UserInfo, Result}};
                Other ->
                  ?FILE_LOG_ERROR("query friend stage info error, reason = ~p, sql = ~p", [Other, NSql]),
                  {fail, "query data error"}
              end;
            _ ->
              ?FILE_LOG_INFO("query friend data: SQL = ~p", [Sql]),
              case mysql:fetch(DbName, Sql) of
                {data, #mysql_result{rows = Rows}} ->
                  Result = lists:map(
                    fun([Uid, UName, DisName]) ->
                      {dd_util:to_integer(Uid), dd_util:to_list(UName), dd_util:to_list(DisName)}
                    end, Rows),
                  {success, {UserInfo, Result}};
                Other ->
                  ?FILE_LOG_ERROR("query friend stage info error, reason = ~p", [Other]),
                  {fail, "query data error"}
              end
          end
      end
  end;
execute(DbName, {check_device, Device}) ->
  case query_user_info_by_device(DbName, Device) of
    {success, UserList} -> {success, UserList};
    not_exist -> not_exist;
    {fail, Reason} -> {fail, Reason}
  end;
execute(DbName, {get_version_info, VersionID}) ->
  Sql = mysql_util:select_query("version", ["version_url", "package_url"], "version_id='" ++ mysql_util:escape(VersionID) ++ "'"),
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows=[]}} -> not_exist;
    {data, #mysql_result{rows = [[Url, PackageUrl]]}} ->
      {success, #version{version_id = VersionID, version_package_url = dd_util:to_list(PackageUrl), version_update_url = dd_util:to_list(Url)}};
    Other ->
      ?FILE_LOG_DEBUG("query_user_info_by_device error => reason = ~p", Other),
      {fail, "query error"}
  end.


%%生成UIN
generate_uin(DbName) ->
  Sql1 = "select uin from uin",
  case mysql:fetch(DbName, Sql1, ?TIME_OUT) of
    {data, #mysql_result{rows = [[Uin]]}} ->
      Sql = "update uin set `uin` = `uin`+1",
      case mysql:fetch(DbName, Sql, ?TIME_OUT) of
        {updated, #mysql_result{affectedrows=1}} -> {success, Uin};
        _ ->
          {fail, "sys error"}
      end;
    {data,#mysql_result{rows=[]}} -> {fail, "sys error"}
  end.

query_user_info_by_uname(DbName, UName) ->
  Sql = mysql_util:select_query("login", ["device", "uin", "uname", "dis_name", "platform_info", "addition", "create_ts"], "uname='" ++ mysql_util:escape(UName) ++ "'"),
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = [UserInfo]}} ->
      {success, database_util:decode_user_info_rd(UserInfo)};
    {data, #mysql_result{rows=[]}} -> not_exist;
    _Other -> fail
  end.
query_user_info_by_device(DbName, Device) ->
  Sql = mysql_util:select_query("login", ["device", "uin", "uname", "dis_name", "platform_info", "addition", "create_ts"], "device='" ++ mysql_util:escape(Device) ++ "'"),
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows=[]}} -> not_exist;
    {data, #mysql_result{rows = [Info]}} ->
      {success, database_util:decode_user_info_rd(Info)};
    Other ->
      ?FILE_LOG_DEBUG("query_user_info_by_device error => reason = ~p", Other),
      {fail, "query error"}
  end.

query_user_info_by_uin(DbName, Uin) ->
  Sql = mysql_util:select_query("login", ["device", "uin", "uname", "dis_name", "platform_info", "addition", "create_ts"], "uin='" ++ dd_util:to_list(Uin) ++ "'"),
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = [UserInfo]}} ->
      {success, database_util:decode_user_info_rd(UserInfo)};
    {data, #mysql_result{rows=[]}} -> fail
  end.

result_to_list(undefined) -> undefined;
result_to_list(Value) -> dd_util:to_list(Value).

create_query_friend_sql(FriendList)->
  create_query_friend_sql_1(FriendList, "select uin, uname, dis_name from login where uname in(", []).
create_query_friend_sql_1([], Sql, OutList) -> {Sql ++ ");", OutList};
create_query_friend_sql_1([Last], Sql, OutList) ->
  case Last#friend_item.id of
    ""->
      create_query_friend_sql_1([], Sql, OutList);
    ID ->
      {Sql ++ dd_util:to_list(ID) ++ ");", [ID | OutList]}
  end;
create_query_friend_sql_1([H|T], Sql, OutList) ->
  case H#friend_item.id of
    "" ->
      create_query_friend_sql_1(T, Sql, OutList);
    ID ->
      create_query_friend_sql_1(T, Sql ++ ID ++ ", ", [ID | OutList])
  end.


