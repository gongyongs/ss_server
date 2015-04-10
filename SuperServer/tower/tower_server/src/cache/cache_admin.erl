%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2014 下午3:24
%%%-------------------------------------------------------------------
-module(cache_admin).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
%% API
-export([
  handler_action/4,
  handler_action/3,
  get_user_info/2,
  handler_get/3,
  backpack_oper/3,
  del_account/3
]).

%handler_action({Action,Uin,Present_num,Present_id_1})when is_integer(Present)

handler_action(Action,Uin, Data, Oper) ->
  ?FILE_LOG_DEBUG("enter cache_admin=>handler_action=>Action:~p ", [Action]),
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, admin_proc, {Uin, {Action, Data, Oper}}).

handler_action(Action,Uin, Present_info)when is_tuple(Present_info) ->
  ?FILE_LOG_DEBUG("enter cache_admin:~p ", [Action]),
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, admin_proc, {Uin, {Action, Present_info}});

handler_action(Action,Uin, Data) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, admin_proc, {Uin, {Action, Data}}).

handler_get(Action,Uin, Data) ->
  ?FILE_LOG_DEBUG("enter cache server GET :~p ", [Action]),
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, admin_get, {Uin, {Action, Data}}).

get_user_info(Uin,TypeList) ->                                            %  [Uin, [get_user_info,Type]]
  ?FILE_LOG_DEBUG("enter cache_admin => get_user_info ", []),
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, admin_get, {Uin, {get_user_info,TypeList}}).      %查询数据

backpack_oper(Uin, Action, Data) ->
  ?FILE_LOG_DEBUG("backpack_oper => ~p, ~p, ~p", [Uin, Action, Data]),
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, admin_proc, {Uin, {Action, Data}}).      %查询数据

del_account(Uin, Action, Data) ->
  ?FILE_LOG_DEBUG("del account => ~p", [Uin]),
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, admin_del, {Uin, {Action, Data}}).      %%删除


