%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 十一月 2014 下午6:55
%%%-------------------------------------------------------------------
-module(login_ios).
-author("zqlt").

-include("../login.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-define(SINGLE_MAX_COUNT, 5).

%% API
-export([execute/2]).

execute(login_by_device, Device) ->
  ?FILE_LOG_DEBUG("login_by_device [~p]", [Device]),
  device = get(mode),
  key_check(Device),
  TableID = get(table_id),
  DBNode = get_database_node(),
  case ets:lookup(TableID, Device) of
    [] ->
      case rpc:call(DBNode, database_monitor, execute, [login_by_device, Device]) of
        {success, UserInfo} ->
          ets:insert(TableID, UserInfo),
          {success, UserInfo};
        Other ->
          ?FILE_LOG_DEBUG("login_by_device error=> device = ~p, reason = ~p", [Device, Other]),
          {fail, "HintSystemError"}
      end;
    [UserInfo] ->
      ets:update_element(TableID, Device, {#user_info.add_ts, dd_util:timestamp()}),
      {success, UserInfo}
  end;


execute(login_by_uname, {Plat, Device, PlayerID, DisName, Friend}) ->
  ?FILE_LOG_DEBUG("login_by_name[~p,~p, ~p, ~p, ~p]", [Plat, Device, PlayerID, DisName, Friend]),
  uname = get(mode),
  key_check(PlayerID),
  TableID = get(table_id),
  DBNode = get_database_node(),
  case ets:lookup(TableID, PlayerID) of
    [] ->
      case rpc:call(DBNode, database_monitor, execute, [login_by_uname, {PlayerID, Device}]) of
        {success, UserInfo} ->
          NUserInfo = UserInfo#user_info{dis_name = DisName, add_ts = dd_util:timestamp(), platform_info = UserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list}},
          success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
          ets:insert(TableID, NUserInfo),
          {success, NUserInfo};
        Other ->
          ?FILE_LOG_ERROR("login_by_uname error, reason = ~p", [Other]),
          {fail, "HintSystemError"}
      end;
    [UNameUserInfo] ->
      %%检查账户是否绑定设备
      case UNameUserInfo#user_info.device of
        "" -> %%未绑定设备
          case rpc:call(DBNode, database_monitor, execute, [login_by_uname, {PlayerID, Device}]) of
            {success, UserInfo} ->
              NUserInfo = UserInfo#user_info{dis_name = DisName, add_ts = dd_util:timestamp(), platform_info = UserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list}},
              success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
              ets:insert(TableID, NUserInfo),
              {success, NUserInfo};
            Other ->
              ?FILE_LOG_ERROR("login_by_uname error, reason = ~p", [Other]),
              {fail, "HintSystemError"}
          end;
        _ ->  %%该玩家已绑定设备
          NUserInfo = UNameUserInfo#user_info{dis_name = DisName, add_ts = dd_util:timestamp(), platform_info = UNameUserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list}},
          success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
          ets:insert(TableID, NUserInfo),
          {success, NUserInfo}
      end
  end;

execute(update_by_device, Device) ->
  ?FILE_LOG_DEBUG("update_by_device[~p]", [Device]),
  device = get(mode),
  key_check(Device),
  TableID = get(table_id),
  DBNode = get_database_node(),
  ets:delete(TableID, Device),
  case rpc:call(DBNode, database_monitor, execute, [get_user_by_device, Device]) of
    {success, UserInfo} ->
      ets:insert(TableID, UserInfo),
      success;
    Other ->
      ?FILE_LOG_ERROR("get_user_by_device error, dev = ~p, reason = ~p", [Device, Other]),
      fail
  end;

execute(update_by_uname, UName) ->
  ?FILE_LOG_DEBUG("update_by_uname[~p]", [UName]),
  uname = get(mode),
  key_check(UName),
  TableID = get(table_id),
  DBNode = get_database_node(),
  ets:delete(TableID, UName),
  case rpc:call(DBNode, database_monitor, execute, [get_user_by_uname, UName]) of
    {success, UserInfo} ->
      ets:insert(TableID, UserInfo),
      success;
    Other ->
      ?FILE_LOG_ERROR("get_user_by_uname error, dev = ~p, reason = ~p", [UName, Other]),
      fail
  end.


get_database_node() ->
  case ets:lookup(login_cfg, database_node) of
    [] ->
      ?FILE_LOG_DEBUG("database node not exist", []),
      throw({custom, "HintSystemError"});
    [#login_cfg{value = Node}] -> Node
  end.

get_hash_rule() ->
  case ets:lookup(login_cfg, hash_rule) of
    [#login_cfg{value = HashRule}] -> {success, HashRule};
    [] -> throw({custom, "get_hash_rule fail"})
  end.


get_key_by_mod(UserInfo) when is_record(UserInfo, user_info) ->
  case get(mode) of
    uname -> UserInfo#user_info.uname;
    device -> UserInfo#user_info.device
  end.

%%检查节点映射
key_check(Key) ->
  {success, HashRule} = get_hash_rule(),
  SelfNode = node(),
  case hash_service_util:find_key_store_node(Key, HashRule) of
    {success, SelfNode} -> ok;
    _ ->
      throw({custom, "hash_rule exception"})
  end.
