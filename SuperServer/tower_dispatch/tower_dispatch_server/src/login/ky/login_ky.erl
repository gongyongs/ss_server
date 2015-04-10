%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 一月 2015 下午5:22
%%%-------------------------------------------------------------------
-module(login_ky).
-author("zqlt").
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../login.hrl").

%% API
-export([execute/2]).

execute(login_by_uname, {UName, "KY", DisName, Device}) ->
  ?FILE_LOG_DEBUG("login_by_uname => uname = ~p, plat = ~p, dis_name = ~p, device = ~p", [UName, "KY", DisName, Device]),
  uname = get(mode),
  key_check(UName),
  TableID = get(table_id),
  DBNode = get_database_node(),
  case ets:lookup(TableID, UName) of
    [] ->
      case rpc:call(DBNode, database_monitor, execute, [login_by_uname, UName]) of
        {success, UserInfo} ->
          NUserInfo = UserInfo#user_info{uname = UName, dis_name = DisName, device = Device, addition = #addition{}, platform_info = #platform{player_dis_name = DisName, plat_friend = [],
            player_id = UName, plat_type = "KY"}, add_ts = dd_util:timestamp()},
          ets:insert(TableID, NUserInfo),
          success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
          {success, NUserInfo};
        CROther ->
          ?FILE_LOG_DEBUG("create_user_by_uname error => uname = ~p, reason = ~p", [UName, CROther]),
          {fail, "HintSystemError"}
      end;
    [UNameUserInfo] ->
      if
        DisName =:= UNameUserInfo#user_info.dis_name -> ok;      %%检查玩家id是否有变化，主要是指NickName
        true ->
          NUNameUserInfo = UNameUserInfo#user_info{uname = UName, dis_name = DisName, device = Device, addition = #addition{}, platform_info = #platform{player_dis_name = DisName, plat_friend = [],
            player_id = UName, plat_type = "KY"}, add_ts = dd_util:timestamp()},
          success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUNameUserInfo]),
          ets:insert(TableID, NUNameUserInfo)
      end,
      {success, UNameUserInfo}
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

%%检查节点映射
key_check(Key) ->
  {success, HashRule} = get_hash_rule(),
  SelfNode = node(),
  case hash_service_util:find_key_store_node(Key, HashRule) of
    {success, SelfNode} -> ok;
    _ ->
      throw({custom, "hash_rule exception"})
  end.



