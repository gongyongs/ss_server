%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 七月 2014 下午2:54
%%%-------------------------------------------------------------------
-module(login_impl).
-author("zqlt").

-include("login.hrl").
-include("../../deps/file_log/include/file_log.hrl").

%% API
-export([
  execute/2
]).

%%绑定设备
execute(bond_device, {Plat, Device, PlayerID, DisName, Friend}) ->
  ?FILE_LOG_DEBUG("bonde_device [~p]", [Device]),
  key_check(PlayerID),
  DBNode = get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [bond_device, {Plat, Device, PlayerID, DisName, Friend}]) of
    {success, UserInfo} ->
      notify_update(Device, PlayerID),
      {success, UserInfo};
    account_has_bond ->  account_has_bond;
    GTOther ->
      ?FILE_LOG_DEBUG("bond_device error => reason = ~p, device = ~p", [GTOther, Device]),
      {fail, "HintSystemError"}
  end;

execute(check_device, Device) ->
  ?FILE_LOG_DEBUG("check_device [~p]", [Device]),
  key_check(Device),
  DBNode = get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [check_device, Device]) of
    not_exist -> {success, 0};
    {success, UserInfo} ->
      case UserInfo#user_info.uname of
        "" -> {success, 0};
        _ -> {success, 1}
      end;
    Other ->
      ?FILE_LOG_DEBUG("check_device error, device = ~p, reason = ~p", [Device, Other]),
      {fail, "HintSystemError"}
  end.

notify_update(Device, UName) ->
  login_work:update_by_device(Device, login_ios),
  Node = node(),
  LoginNode = find_data_node(UName),
  case LoginNode of
    Node -> login_work:update_by_uname(UName, login_ios);
    _ -> rpc:cast(LoginNode, login_work, update_by_uname, [UName, login_ios])
  end.

get_database_node() ->
  case ets:lookup(login_cfg, database_node) of
    [] ->
      ?FILE_LOG_DEBUG("database node not exist", []),
      throw({custom, "HintSystemError"});
    [#login_cfg{value = Node}] -> Node
  end.


find_data_node(Key) when is_list(Key) ->
  {success, LoginHashRule} = get_hash_rule(),
  case hash_service_util:find_key_store_node(Key, LoginHashRule) of
    fail -> throw({custom, "not available data nodes 2"});
    {success, Node} -> {success, Node}
  end.


get_hash_rule() ->
  case ets:lookup(login_cfg, hash_rule) of
    [#login_cfg{value = HashRule}] -> {success, HashRule};
    [] -> throw({custom, "get_hash_rule fail"})
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




