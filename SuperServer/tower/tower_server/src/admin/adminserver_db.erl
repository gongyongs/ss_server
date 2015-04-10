-module(adminserver_db).
-author("j").
-include("../../deps/file_log/include/file_log.hrl").
-include("../dd_ms.hrl").
%% API
-export([
  get_uins_above_pay/1,
  add_shop_item/1,
  delete_shop_item/1,
  query_shop_config/0,
  change_model_state/1,
  notice_manager/2,
  query_all_notice/0,
  query_notice_by_id/1
]).

change_model_state(Action)->
  DBNode = adminserver_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [change_model_state, Action]) of
    success->success;
    {fail,Reason} ->
      ?FILE_LOG_ERROR("adminserver_db: change_model_state error! Reason is ~p", [Reason]),
      {fail,Reason}
  end.

get_uins_above_pay(Pay_count)  ->
  DBNode = adminserver_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [get_uins_above_pay, Pay_count]) of
    {success, Result}->{success, Result};
    fail ->
      ?FILE_LOG_ERROR("adminserver_db: get_user_above_pay error! ", []),
      fail
  end.

add_shop_item(Item)->
  {success, CacheNodeList} = dd_ms:get_all_cache(),
  try
    Result = rpc_all_cache(CacheNodeList, {cache_configure_data, add_shop_goods, [Item]}),
    success = result_check(Result),
    DBNode = adminserver_util:get_database_node(),
    ?FILE_LOG_DEBUG("get_database_node ok",[]),
    case rpc:call(DBNode, database_monitor, execute, [add_shop_item, Item]) of
      success->success;
      fail ->
        ?FILE_LOG_ERROR("adminserver_db:add_shop_item error! ", []),
        {fail,"add_shop_item error"}
    end
  catch
    {custom, Reason} ->
      {fail, Reason};
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      {fail, "system error"}
  end.

delete_shop_item(ID)->
  {success, CacheNodeList} = dd_ms:get_all_cache(),
  try
    Result = rpc_all_cache(CacheNodeList, {cache_configure_data, del_commodity_config, [ID]}),
    success = result_check(Result),
    DBNode = adminserver_util:get_database_node(),
    ?FILE_LOG_DEBUG("get_database_node ok",[]),
    case rpc:call(DBNode, database_monitor, execute, [delete_shop_item, ID]) of
      success->success;
      fail ->
        ?FILE_LOG_ERROR("adminserver_db:del_shop_item error! ", []),
        {fail,"del_shop_item error"}
    end
  catch
    {custom, Reason} ->
      {fail, Reason};
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      {fail, "system error"}
  end.

query_shop_config()->
  {success,CacheNode} = adminserver_util:get_cache_node(1),
  ?FILE_LOG_DEBUG("get_cache_node ok",[]),
  case rpc:call(CacheNode, cache_configure_data, query_all_shop_goods, []) of
    {success, {CsvConfig, DbConfig}}-> {success, {CsvConfig, DbConfig}};
    {fail,Reason} ->
      ?FILE_LOG_ERROR("adminserver_db error! ", []),
      {fail,Reason}
  end.

rpc_all_cache(List, Param) ->
  rpc_all_cache_1(List, Param, []).

rpc_all_cache_1([], {_Mod, _Func, _FuncParam}, OutRetList) -> OutRetList;
rpc_all_cache_1([NodeInfo | T], {Mod, Func, FuncParam}, OutList) ->         %%{cache_configure_data, del_commodity_config, [ID]}
  Ret =
    case rpc:call(NodeInfo#cache.node, Mod, Func, FuncParam) of
      {badrpc, Reason} ->
        ?FILE_LOG_ERROR("rpc_all_cache error, mod = ~p, func = ~p, param = ~p, reason = ~p", [Mod, Func, FuncParam, Reason]),
        fail;
      Other -> Other
    end,
  rpc_all_cache_1(T, {Mod, Func, FuncParam}, [Ret | OutList]).

result_check([]) -> success;
result_check([Ret | _T]) ->
  case Ret of
    fail -> throw({custom, "HintSystemError"});
    {fail, _} ->  throw({custom, "HintSystemError"});
    {success, _} -> success;
    success -> success
  end.


notice_manager(add_notice, Notice) ->
  DBNode = adminserver_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [insert_notice, Notice]) of
    success -> success;
    Other ->
      ?FILE_LOG_ERROR("insert new notice error, reason = ~p", [Other]),
      fail
  end;
notice_manager(modify_notice, Notice) ->
  DBNode = adminserver_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [update_notice, Notice]) of
    success -> success;
    Other ->
      ?FILE_LOG_ERROR("modify notice error, reason = ~p", [Other]),
      fail
  end;
notice_manager(delete_notice, NoticeID) ->
  DBNode = adminserver_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [delete_notice, NoticeID]) of
    success -> success;
    Other ->
      ?FILE_LOG_ERROR("modify notice error, reason = ~p", [Other]),
      fail
  end.

query_all_notice() ->
  DBNode = adminserver_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [get_notice_rd, 0]) of
    {success, NoticeList} -> {success, NoticeList};
    Other ->
      ?FILE_LOG_ERROR("modify notice error, reason = ~p", [Other]),
      fail
  end.

query_notice_by_id(ID) ->
  DBNode = adminserver_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [get_notice_by_id, ID]) of
    {success, Notice} -> {success, Notice};
    Other ->
      ?FILE_LOG_ERROR("modify notice error, reason = ~p", [Other]),
      fail
  end.





