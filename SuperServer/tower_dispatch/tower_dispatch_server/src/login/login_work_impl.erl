%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十二月 2014 下午8:38
%%%-------------------------------------------------------------------
-module(login_work_impl).
-author("zqlt").

-include("login.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-define(SINGLE_MAX_COUNT, 5).
%% API
-export([data_trans/0]).


data_trans() ->
  %%获取最新的哈希规则
  {success, HashRule} = get_hash_rule(),
  TableId = get(table_id),
  SelfNode = node(),
  {TargetNode, {RdList, Len}} =
    ets:foldl(
      fun(UserInfo, {TmpTargetNode, {TmpRdList, TmpLen}}) ->
        {success, StorageNode} = hash_service_util:find_key_store_node(get_key_by_mod(UserInfo), HashRule),
        if
          SelfNode =:= StorageNode ->			%%不用转移数据
            {TmpTargetNode, {TmpRdList, TmpLen}};
          true -> 							%%%%需要转移，删除本节点的记录
            ets:delete(TableId, get_key_by_mod(UserInfo)),
            NewTmpTargetNode =
              if
                TmpTargetNode =:= undefined -> StorageNode;
                true ->
                  if
                    TmpTargetNode =/= StorageNode ->
                      ?FILE_LOG_WARNING("dest_node=~p, storage_node=~p", [TmpTargetNode, StorageNode]),
                      throw({custom, "logic exception"});
                    true -> StorageNode
                  end
              end,
            RdLen = length(TmpRdList),
            if
              RdLen =:= ?SINGLE_MAX_COUNT ->
                %%数据先发出去
                ?FILE_LOG_DEBUG("send data[~p] to [~p]", [RdLen, NewTmpTargetNode]),
                rpc:call(NewTmpTargetNode, login_work, data_trans, [{get(mode), node()}, TmpRdList]),
                {NewTmpTargetNode, {[UserInfo], 1}};
              true ->
                {NewTmpTargetNode, {[UserInfo|TmpRdList], TmpLen + 1}}
            end
        end
      end, {undefined, {[], 0}}, TableId),
  if
    Len > 0 ->
      ?FILE_LOG_DEBUG("send data[~p] to [~p]", [Len, TargetNode]),
      rpc:call(TargetNode, login_work, data_trans, [{get(mode), node()}, RdList]);
    true -> ok
  end,
  success.

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
