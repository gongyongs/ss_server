%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 七月 2014 上午10:12
%%%-------------------------------------------------------------------
-module(cache_data_proc).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
-include("../csv.hrl").
-include("cache_def.hrl").
-include("cache_data.hrl").

-define(SINGLE_MAX_COUNT, 5).
-define(PRESERVE_TIME, 15*60).
-define(EXPIRED_TIME, 2*60*60).

-define(INIT_FIELD, ["gold_coin"
					, "gem"
					, "platform_info"
					, "hero"
					, "backpack"
					, "mission"
					, "achievement"
					,"shop"
					, "stage"
					, "login_reward"
					, "strength"
					, "lottery"
					, "addition"
					, "create_ts"
					, "last_login_ts"
					, "reward_match"
					, "reward_match_ts"]).
%% API
-export([
  execute/3,
  save_player_data/1,
  timer_save_player/1,
  clean_expire_data/1,
  stat_power_rank/2,
  stat_pay_rank/2
]).

execute(TableID, get_account, Uin) when is_integer(Uin) ->
  ?FILE_LOG_DEBUG("cache_data_proc: get_account [~p]", [Uin]),
  key_check(Uin),
  case ets:lookup(TableID, Uin) of
    [] ->
      %%缓存中没有，从数据库中获取
      ?FILE_LOG_DEBUG("cache_data_proc: get account from db [~p]", [Uin]),
      DbNode = get_cfg(database_node),
      {success, {Account, IsCreate}} = rpc:call(DbNode, database_monitor, execute, [get_account, {Uin, true}]),
      case IsCreate of
        true ->
          NewAccount = cache_account_def:init_account_from_config(Account),
          %%更新数据库
          FieldList = ?INIT_FIELD,
          %%success = rpc:call(DbNode, database_monitor, execute, [update_account, {NewAccount, FieldList}]),
          ets:insert(TableID, #cache_account{uin = NewAccount#account.uin, account = NewAccount, add_ts = dd_util:timestamp(), field_list = FieldList, save_ts = 0}),
          {success, {NewAccount, true}};
        false ->
          ets:insert(TableID, #cache_account{uin = Account#account.uin, account = Account, add_ts = dd_util:timestamp(), field_list = [], save_ts = dd_util:timestamp()}),
          {success, {Account, IsCreate}}
      end;
    [CacheAccount] ->
      refresh_time(TableID, Uin),
      {success, {CacheAccount#cache_account.account, false}}
  end;

execute(TableID, query_account_without_create, {Uin, FieldList}) when is_integer(Uin) andalso is_list(FieldList) ->
  ?FILE_LOG_DEBUG("cache_data_proc: query_account_without_create uin = ~p, field list = ~p", [Uin, FieldList]),
  key_check(Uin),
  case ets:lookup(TableID, Uin) of
    [] ->
      DbNode = get_cfg(database_node),
      case rpc:call(DbNode, database_monitor, execute, [get_account, {Uin, false}]) of
        {success, {Account, _}} ->
          ets:insert(TableID, #cache_account{uin = Account#account.uin, account = Account, add_ts = dd_util:timestamp(), field_list = [], save_ts = dd_util:timestamp()}),
          {success, query_account_data(Account, FieldList)};
        Other ->
          ?FILE_LOG_ERROR("query account [~p] error, reason = ~p", [Uin, Other]),
          {fail, "HintSystemDataError"}
      end;
    [CacheAccount] -> {success,query_account_data(CacheAccount#cache_account.account, FieldList)}
  end;

execute(TableID, get_account_without_create, Uin) when is_integer(Uin) ->
  ?FILE_LOG_DEBUG("cache_data_proc: get_account_without_create uin = ~p", [Uin]),
  key_check(Uin),
  case ets:lookup(TableID, Uin) of
    [] ->
      DbNode = get_cfg(database_node),
      case rpc:call(DbNode, database_monitor, execute, [get_account, {Uin, false}]) of
        {success, {Account, _}} ->
          ets:insert(TableID, #cache_account{uin = Account#account.uin, account = Account, add_ts = dd_util:timestamp(), field_list = [], save_ts = dd_util:timestamp()}),
          {success, Account};
        Other ->
          ?FILE_LOG_ERROR("query account [~p] error, reason = ~p", [Uin, Other]),
          {fail, "HintSystemDataError"}
      end;
    [CacheAccount] -> {success, CacheAccount#cache_account.account}
  end;

execute(TableID, update_account, {Account, FieldList, false}) when is_record(Account, account) andalso is_list(FieldList)->   %%不需要立即存储，定时存储
  ?FILE_LOG_DEBUG("cache_data_proc:update_account [~p]", [Account#account.uin]),
  key_check(Account#account.uin),
  case ets:lookup(TableID, Account#account.uin) of
    [] ->
      UpdateField = dd_util:filter_string(lists:merge(FieldList, [])),
      NCacheAccount = #cache_account{uin = Account#account.uin, account = Account, add_ts = dd_util:timestamp(), field_list = UpdateField, save_ts = 0},
      ets:insert(TableID, NCacheAccount);
    [CacheAccount] ->
      UpdateField = dd_util:filter_string(lists:merge(FieldList, CacheAccount#cache_account.field_list)),
      NCacheAccount = CacheAccount#cache_account{uin = Account#account.uin, add_ts = dd_util:timestamp(), account = Account, field_list = UpdateField},
      ets:insert(TableID, NCacheAccount)
  end,
  success;
execute(TableID, update_account, {Account, FieldList, true}) when is_record(Account, account) andalso is_list(FieldList)->    %%立即存储数据库
  ?FILE_LOG_DEBUG("cache_data_proc:update_account immediately [~p]", [Account#account.uin]),
  key_check(Account#account.uin),
  case ets:lookup(TableID, Account#account.uin) of
    [] ->
      UpdateField = dd_util:filter_string(lists:merge(FieldList, [])),
      NCacheAccount = #cache_account{uin = Account#account.uin, account = Account, add_ts = dd_util:timestamp(), field_list = UpdateField, save_ts = 0},
      ets:insert(TableID, NCacheAccount),
      {success, _} = save_player_to_db(NCacheAccount);
    [CacheAccount] ->
      UpdateField = dd_util:filter_string(lists:merge(FieldList, CacheAccount#cache_account.field_list)),
      NCacheAccount = CacheAccount#cache_account{uin = Account#account.uin, add_ts = dd_util:timestamp(), account = Account, field_list = UpdateField},
      ets:insert(TableID, NCacheAccount),
      {success, _} = save_player_to_db(NCacheAccount)
  end,
  success;

execute(TableID, insert_account, Account) when is_record(Account, account) ->
  ?FILE_LOG_DEBUG("cache_data_proc: insert_account[~p]", [Account#account.uin]),
  key_check(Account#account.uin),
  CacheAccount = #cache_account{uin = Account#account.uin, add_ts = dd_util:timestamp(), account = Account, field_list = ?INIT_FIELD, save_ts = 0},
  ets:insert(TableID, CacheAccount),
  success;

%%保存整张表中玩家的数据，有可能进程退出
execute(TableID, save_player_data, _) ->
  ?FILE_LOG_DEBUG("cache_data_proc: save_player_data", []),
  save_player_data(TableID);

%%保存数据
execute(TableID, save_player, {Account, FieldList}) when is_record(Account, account) ->
  ?FILE_LOG_DEBUG("cache_data_proc: save_player[~p]", [Account#account.uin]),
  key_check(Account#account.uin),
  UpdatedFieldList =
    case ets:lookup(TableID, Account#account.uin) of
      [] -> FieldList;
      [CacheAccount] ->
        dd_util:filter_string(lists:merge(CacheAccount#cache_account.field_list, FieldList))
    end,
  case UpdatedFieldList of
    [] -> success;
    _ ->
      DbNode = get_cfg(database_node),
      success = rpc:call(DbNode, database_monitor, execute, [update_account, {Account, UpdatedFieldList}], 20000),
      NCacheAccount = #cache_account{uin = Account#account.uin, add_ts = dd_util:timestamp(), field_list = [], save_ts = dd_util:timestamp(), account = Account},
      ets:insert(TableID, NCacheAccount),
      success
  end;

execute(TableID, data_transfer, _) ->
  ?FILE_LOG_DEBUG("cache_data_proc: data_transfer", []),
  HashRule = get_cfg(hash_rule),
  DbNode = get_cfg(database_node),
  SelfNode = node(),
  %%遍历列表查找不在该节点的数据，并转移
  {TargetNode, AccountList} =
    ets:foldl(
      fun(CacheAccount, {TempTargetNode, TmpAccountList}) ->
        {success, Node} = hash_service_util:find_key_store_node(dd_util:to_list(CacheAccount#cache_account.account#account.uin), HashRule),
        case Node of
          SelfNode  -> {TempTargetNode, TmpAccountList};
          _ ->
            %%删除之前保存数据库, 在删除之前，保存数据库
            case CacheAccount#cache_account.field_list of
              [] -> success;
              FieldList ->
                UpdatedFieldList = dd_util:filter_string(FieldList),
                success = rpc:call(DbNode, database_monitor, execute, [update_account, {CacheAccount#cache_account.account, UpdatedFieldList}], 20000)
            end,

            %%删除记录，转移节点
            ets:delete(TableID, CacheAccount#cache_account.uin),

            NTargetNode =
              case TempTargetNode of
                undefined -> Node;
                Node -> Node;
                _ ->
                  ?FILE_LOG_ERROR("data trans error, dest node = ~p, storage node = ~p", [TempTargetNode, Node]),
                  throw({custom, "HintSystemError"})
              end,
            case length(TmpAccountList) of
              ?SINGLE_MAX_COUNT ->                 %%发送
                ?FILE_LOG_DEBUG("send data len = ~p, to node = ~p", [?SINGLE_MAX_COUNT, NTargetNode]),
                rpc:call(NTargetNode, cache_data_mgr, data_transfer, [node(), TmpAccountList]),
                {NTargetNode, [CacheAccount#cache_account.account]};
              _ ->
                {NTargetNode, [CacheAccount#cache_account.account | TmpAccountList]}
            end
        end
      end, {undefined, []}, TableID),

  %%传输结束，将剩余的数据传输
  Len = length(AccountList),
  if
    Len > 0 ->
      ?FILE_LOG_DEBUG("send data len = ~p, to node = ~p", [Len, TargetNode]),
      rpc:call(TargetNode, cache_data_mgr, data_transfer, [node(), AccountList]);
    true -> ok
  end,
  success;


execute(TableID, delete_account, Uin) when is_integer(Uin) ->
  ?FILE_LOG_DEBUG("cache_data_proc:delete account [~p]", [Uin]),
  DbNode = get_cfg(database_node),
  success = rpc:call(DbNode, database_monitor, execute, [delete_account, Uin], 10000),
  ets:delete(TableID, Uin),
  success;

execute(TableID, delete_account_from_cache, Uin) ->
  ?FILE_LOG_DEBUG("delete_account_from_cache: uin = ~p", [Uin]),
  Ret = ets:delete(TableID, Uin),
  ?FILE_LOG_DEBUG("delete account from cache ret = ~p", [Ret]),
  success;

execute(_TableID, get_charge_order, {Uin, Order}) when is_integer(Uin) andalso is_list(Order) ->
  DbNode = get_cfg(database_node),
  case rpc:call(DbNode, database_monitor, execute, [get_charge_order, {Uin, Order}]) of
    not_exist -> not_exist;
    {success, Item} -> {success, Item};
    {fail, Reason} -> {fail, Reason};
    _Other -> {fail, "logic error"}
  end;

execute(_TableID, add_charge_order, {Uin, Order}) when is_integer(Uin) andalso is_list(Order) ->
  DbNode = get_cfg(database_node),
  case rpc:call(DbNode, database_monitor, execute, [add_charge_order, {Uin, Order}]) of
    {badrpc, _Reason} -> fail;
    Ret -> Ret
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_player_data(TableID) ->
  DbNode = get_cfg(database_node),
  {SuccessCount, FailAccountList} =
    ets:foldl(
      fun(CacheAccount, {TempSuccessCnt, FailList}) ->
        case CacheAccount#cache_account.field_list of
          [] -> {TempSuccessCnt + 1, FailList};
          FieldList ->
            UpdateField = dd_util:filter_string(FieldList),
            case rpc:call(DbNode, database_monitor, execute, [update_account, {CacheAccount#cache_account.account, UpdateField}], 20000) of
              success -> {TempSuccessCnt + 1, FailList};
              Other ->
                ?FILE_LOG_ERROR("update account error, uin = ~p, reason = ~p", [CacheAccount#cache_account.uin, Other]),
                {TempSuccessCnt, [CacheAccount | FailList]}
            end
        end
      end, {0, []}, TableID),
  ?FILE_LOG_INFO("save_player_data, success count: ~p, fail count: ~p", [SuccessCount, length(FailAccountList)]),
  %%将失败的account写入到文件中
  write_cache_error_log(FailAccountList, TableID),
  success.

timer_save_player(TableID) ->
  try
    ?FILE_LOG_DEBUG("cache_data_proc: time_to_save_player, table count = ~p", [dd_util:get_ets_size(TableID)]),
    DbNode = get_cfg(database_node),
    CurTime = dd_util:timestamp() - ?PRESERVE_TIME,
    MatchSpec = [{#cache_account{save_ts = '$1', _='_'}, [{'=<', '$1', CurTime}], ['$_']}],
    case ets:select(TableID, MatchSpec) of
      [] -> success;
      PreserveList ->
        SubList = lists:sublist(PreserveList, 10),
        ?FILE_LOG_DEBUG("cache_data_proc: time_to_save_player, save count = ~p", [length(SubList)]),
        lists:map(
          fun(CacheAccount) ->
            try
              case CacheAccount#cache_account.field_list of
                [] ->
                  ets:update_element(TableID, CacheAccount#cache_account.uin, [{#cache_account.save_ts, dd_util:timestamp()}]);
                FieldList ->
                  ?FILE_LOG_INFO("SAVE!!! Uin = ~p", [CacheAccount#cache_account.uin]),
                  UpdateField = dd_util:filter_string(FieldList),
                  success = rpc:call(DbNode, database_monitor, execute, [update_account, {CacheAccount#cache_account.account, UpdateField}], 10000),
                  NCacheAccount = CacheAccount#cache_account{field_list = [], save_ts = dd_util:timestamp()},
                  ets:insert(TableID, NCacheAccount)
              end
            catch
              What:Type ->
                ?FILE_LOG_ERROR("save account error, uin = ~p, what = ~p, type = ~p, stack = ~p", [CacheAccount#cache_account.uin, What, Type, erlang:get_stacktrace()]),
                ok
            end
          end, SubList)
    end
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_ERROR("timer_save_player error, reason = ~p", [Reason]),
      {fail, "HintSystemError"};
    What:Type ->
      ?FILE_LOG_ERROR("execute preserve_rd => error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      {fail, "HintSystemError"}
  end.

clean_expire_data(TableID) ->
  try
    CurTime = dd_util:timestamp() - ?EXPIRED_TIME,
    MatchSpec = [{#cache_account{add_ts = '$1', _='_'}, [{'=<', '$1', CurTime}], ['$_']}],
    case ets:select(TableID, MatchSpec) of
      [] -> success;
      DeleteList ->
        ?FILE_LOG_DEBUG("cache_data process ~p delete rd count ~p", [self(), length(DeleteList)]),
        lists:map(
          fun(CacheAccount) ->
            try
              save_player_to_db(CacheAccount),
              ets:delete(TableID, CacheAccount#cache_account.uin)
            catch
              What:Type ->
                ?FILE_LOG_ERROR("preserve account error, account uin = ~p, what = ~p, type = ~p, stack = ~p", [CacheAccount#cache_account.uin, What, Type, erlang:get_stacktrace()]),
                ok
            end
          end, DeleteList)
    end
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_ERROR("clean_expire_data error, reason = ~p", [Reason]),
      {fail, "HintSystemError"};
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()])
  end.

save_player_to_db(CacheAccount) when is_record(CacheAccount, cache_account)->
  DbNode = get_cfg(database_node),
  case CacheAccount#cache_account.field_list of
    [] -> {success, CacheAccount#cache_account{save_ts = dd_util:timestamp()}};
    FieldList ->
      UpdateList = dd_util:filter_string(FieldList),
      success = rpc:call(DbNode, database_monitor, execute, [update_account, {CacheAccount#cache_account.account, UpdateList}], 10000),
      NCacheAccount = CacheAccount#cache_account{field_list = [], save_ts = dd_util:timestamp()},
      {success, NCacheAccount}
  end.

stat_pay_rank(Index, Len) when is_integer(Index) andalso is_integer(Len) ->
  try
    DbNode = get_cfg(database_node),
    case rpc:call(DbNode, database_monitor, execute, [stat_get_pay_power_playdays, {Index, Len}]) of
      {success, ResultList} ->
        StatList = lists:map(
          fun({Uin, DisName, Hero, Shop, _, Stage}) ->
            TotalStar = lists:foldl(fun(Item, TmpSum) -> TmpSum + Item#tollgate.max_star end, 0, Stage#stage.base_tollgate_list),
            Atk = cache_api:get_user_atk(Hero#heros.character_lists, cache_api:get_equipment_max_level_by_star_count(TotalStar)),
            PayVal = Shop#shop.pay_info#pay_info.total_pay_val,
            PayNum = length(Shop#shop.pay_info#pay_info.pay_record),
            {Uin, DisName, Atk, PayVal, PayNum}
          end, ResultList),
        {success, StatList};
      Other ->
        ?FILE_LOG_DEBUG("stat_pay_rank error, reason = ~p", [Other]) ,
        fail
    end
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_ERROR("stat pay rank error, reason = ~p", [Reason]),
      {fail, "HintSystemError"};
    What:Type ->
      ?FILE_LOG_ERROR("stat pay rank error, what = p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      {fail, "HintSysteError"}
  end.

stat_power_rank(Index, Len) when is_integer(Index) andalso is_integer(Len) ->
  try
    DbNode = get_cfg(database_node),
    case rpc:call(DbNode, database_monitor, execute, [stat_get_pay_power_playdays, {Index, Len}]) of
      {success, ResultList} ->
        StatList = lists:map(
          fun({Uin, DisName, Hero, Shop, LoginReward, Stage}) ->
            TotalStar = lists:foldl(fun(Item, TmpSum) -> TmpSum + Item#tollgate.max_star end, 0, Stage#stage.base_tollgate_list),
            Atk = cache_api:get_user_atk(Hero#heros.character_lists, cache_api:get_equipment_max_level_by_star_count(TotalStar)),
            PayVal = Shop#shop.pay_info#pay_info.total_pay_val,
            PlayDays = LoginReward#login_reward.total_login_days,
            {Uin, DisName, Atk, PlayDays, PayVal}
          end, ResultList),
        {success, StatList};
      Other ->
        ?FILE_LOG_DEBUG("stat_power_rank error, reason = ~p", [Other]) ,
        fail
    end
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_ERROR("stat_power_rank error, reason = ~p", [Reason]),
      {fail, "HintSystemError"};
    What:Type ->
      ?FILE_LOG_ERROR("stat pay rank error, what = p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      {fail, "HintSysteError"}
  end.


query_account_data(Account, FieldList) when is_record(Account, account) andalso is_list(FieldList) ->
  case FieldList of
    [] -> Account;
    _ ->
      lists:map(
        fun(Field) ->
          if
            Field =:= "platform_info" -> {"platform_info", Account#account.platform_info};
            Field =:= "gold_coin" -> {"gold_coin", Account#account.gold_coin};
            Field =:= "gem" -> {"gem", Account#account.gem};
            Field =:= "hero" -> {"hero", Account#account.heros};
            Field =:= "backpack" -> {"backpack", Account#account.backpack};
            Field =:= "mission" -> {"mission", Account#account.mission};
            Field =:= "achievement" -> {"achievement", Account#account.achievement};
            Field =:= "shop" -> {"shop", Account#account.shop};
            Field =:= "stage" -> {"stage", Account#account.stage};
            Field =:= "login_reward" -> {"login_reward", Account#account.login_reward};
            Field =:= "strength" -> {"strength", Account#account.strength};
            Field =:= "lottery" -> {"lottery", Account#account.lottery};
            Field =:= "guild" -> {"guild", Account#account.guild};
            Field =:= "competitive" -> {"competitive", Account#account.competitive};
            Field =:= "addition" -> {"addition", Account#account.addition};
            Field =:= "create_ts" -> {"create_ts", Account#account.create_ts};
            Field =:= "last_login_ts" -> {"last_login_ts", Account#account.last_login_ts};
			Field =:= "reward_match" -> {"reward_match", Account#account.reward_match};
			Field =:= "reward_match_ts" -> {"reward_match_ts", Account#account.reward_match_ts};
            true ->
              ?FILE_LOG_ERROR("qquery_account_data, error field name = ~p", [Field]),
              throw({custom, "HintSystemDataError"})
          end
        end, FieldList)
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_cfg(Key) when is_atom(Key) ->
  case dd_config:get_cfg(Key) of
    {success, Value} -> Value;
    fail -> throw({custom, "HintSystemDataError"})
  end.

refresh_time(TableID, Uin) ->
  case ets:update_element(TableID, Uin, [{#cache_account.add_ts, dd_util:timestamp()}]) of
    true ->
      ?FILE_LOG_DEBUG("update cache data add time: [~p]", [Uin]);
    false ->
      ?FILE_LOG_DEBUG("not update cache data add time [~p]", [Uin])
  end.

write_cache_error_log(FailAccountList, Index) ->
  Path = os:getenv("CACHE_ERROR_LOG"),
  FileName = Path ++ atom_to_list(node()) ++ "_" ++ dd_util:to_list(Index) ++ "_" ++ dd_util:time_format(),
  case FailAccountList of
    [] -> ok;
    _ ->
      {ok, F} = file:open(FileName, write),
      lists:foreach(
        fun(CacheAccount) ->
          Sql = unicode:characters_to_list(iolist_to_binary(database_util:update_account_sql(CacheAccount#cache_account.account, CacheAccount#cache_account.field_list))),
          V = lists:flatten(io_lib:format("[~p]~ts~n", [dd_util:time_format(), Sql])),
          file:write(F, dd_util:to_binary(V))
        end, FailAccountList),
      file:close(F)
  end.



key_check(Uin) when is_integer(Uin) ->
  HashRule = get_config_param(hash_rule),
  SelfNode = node(),
  case hash_service_util:find_key_store_node(dd_util:to_list(Uin), HashRule) of
    {success, SelfNode} -> ok;
    _ ->
      throw({custom, "hash_rule exception"})
  end.

get_config_param(Key) ->
  case dd_config:get_cfg(Key) of
    {success, Value} -> Value;
    fail -> throw({custom, "get_config_param fail:" ++ atom_to_list(Key)})
  end.



