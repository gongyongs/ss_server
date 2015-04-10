%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 十二月 2014 下午1:17
%%%-------------------------------------------------------------------
-module(cache_data_mgr).
-author("zqlt").

-behaviour(gen_server).

-include("../../deps/file_log/include/file_log.hrl").
-include("cache_def.hrl").
-include("cache_data.hrl").
-include("../../deps/hash_service/src/hash_service.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-export([
  get_account/1,
  update_account/3,
  data_transfer/2,
  save_player/2,
  delete_account/1,
  get_charge_order/2,
  add_charge_order/2,
  query_account_without_create/2,
  get_account_without_create/1,
  delete_account_from_cache/1
]).

-export([
  stat_pay_rank/2,
  stat_power_rank/2,
  preserve_user_data/0,
  update_hash_rule/2
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
get_account(Uin) when is_integer(Uin) ->
  ProcName = cache_sup:hash_uin_to_data(Uin),
  cache_data:execute(ProcName, Uin, get_account, Uin).

query_account_without_create(Uin, FieldList) when is_integer(Uin) andalso is_list(FieldList) ->
  ProcName = cache_sup:hash_uin_to_data(Uin),
  cache_data:execute(ProcName, Uin, query_account_without_create, {Uin, FieldList}).

get_account_without_create(Uin) when is_integer(Uin) ->
  ProcName = cache_sup:hash_uin_to_data(Uin),
  cache_data:execute(ProcName, Uin, get_account_without_create, Uin).

delete_account(Uin) when is_integer(Uin) ->
  ProcName = cache_sup:hash_uin_to_data(Uin),
  cache_data:execute(ProcName, Uin, delete_account, Uin).

delete_account_from_cache(Uin) when is_integer(Uin) ->
  ProcName = cache_sup:hash_uin_to_data(Uin),
  cache_data:execute(ProcName, Uin, delete_account_from_cache, Uin).

get_charge_order(Uin, OrderID) ->
  ProcName = cache_sup:hash_uin_to_data(Uin),
  cache_data:execute(ProcName, Uin, get_charge_order, {Uin, OrderID}).

add_charge_order(Uin, Order) ->
  ProcName = cache_sup:hash_uin_to_data(Uin),
  cache_data:execute(ProcName, Uin, add_charge_order, {Uin, Order}).

%%保存用户数据，会直接将玩家数据写入db，主要用于关键数据的保存
save_player(Account, FieldList) when is_record(Account, account) andalso is_list(FieldList) ->
  ProcName = cache_sup:hash_uin_to_data(Account#account.uin),
  cache_data:execute(ProcName, Account#account.uin, save_player, {Account, FieldList}).

update_account(Account, FieldList, IsImmediate) when is_record(Account, account) andalso is_list(FieldList) andalso is_atom(IsImmediate) ->
  ProName = cache_sup:hash_uin_to_data(Account#account.uin),
  cache_data:execute(ProName, Account#account.uin, update_account, {Account, FieldList, IsImmediate}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_hash_rule(IsAffected, CacheHashRule) when is_record(CacheHashRule, hash_rule) ->
  gen_server:call(?MODULE, {update_hash_rule, {IsAffected, CacheHashRule}}, infinity).

preserve_user_data() ->
  gen_server:call(?MODULE, save_player_data, infinity).

stat_pay_rank(Index, Len) ->
  gen_server:call(?MODULE, {stat_pay_rank, {Index, Len}}).

stat_power_rank(Index, Len) ->
  gen_server:call(?MODULE, {stat_power_rank, {Index, Len}}).

%%数据传输，由于节点的改动，导致hash规则改变
-spec data_transfer(Source :: node(), AccountList :: [#account{}]) -> ok.
data_transfer(Source, AccountList) when is_list(AccountList) ->
  gen_server:cast(?MODULE, {data_transfer, {Source, AccountList}}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {success, DbNode} = dd_ms:read_config(database_node),
  dd_config:write_cfg(database_node, DbNode),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(save_player_data, _From, State) ->
  Ret = save_player_data(),
  {reply, Ret, State};
handle_call({update_hash_rule, {false, HashRule}}, _From, State) ->
  %%更新规则
  dd_config:write_cfg(hash_rule, HashRule),
  {reply, success, State};
handle_call({update_hash_rule, {true, HashRule}}, _From, State) ->
  ?FILE_LOG_DEBUG("update_hash_rule lock success [~p]", [HashRule]),
  dd_config:write_cfg(hash_rule, HashRule),
  cache_data_transfer(),
  ?FILE_LOG_DEBUG("trans_data success", []),
  {reply, success, State};
handle_call({stat_pay_rank, {Index, Len}}, _From, State) ->
  ?FILE_LOG_DEBUG("stat pay rank", []),
  Ret = cache_data_proc:stat_pay_rank(Index, Len),
  {reply, Ret, State};
handle_call({stat_power_rank, {Index, Len}}, _From, State) ->
  ?FILE_LOG_DEBUG("stat_power_rank", []),
  Ret = cache_data_proc:stat_power_rank(Index, Len),
  {reply, Ret, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({data_transfer, {SourceNode, DataList}}, State) ->
  ?FILE_LOG_DEBUG("uname source_node[~p] data_trans len[~p]", [SourceNode, length(DataList)]),
  lists:foreach(
    fun(Account) ->
      ProcName = cache_sup:hash_uin_to_data(Account#account.uin),
      cache_data:execute(ProcName, Account#account.uin, insert_account, Account)
    end, DataList),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%保存所有玩家的数据
save_player_data() ->
  DataProcNameList = cache_sup:get_data_proc_list(),
  SelfPid = self(),
  PidList =
    lists:map(
      fun(ProcName) ->
        spawn(fun() ->
          Ret = cache_data:execute(ProcName, 0, save_player_data, []),
          SelfPid ! {self(), Ret}
        end)
      end, DataProcNameList),
  lists:foreach(
    fun(Pid) ->
      receive
        {Pid, success} -> success;
        {Pid, {fail, _}} ->
          ?FILE_LOG_ERROR("save player data fail", []),
          fail
      end
    end, PidList).

cache_data_transfer() ->
  DataProcNameList = cache_sup:get_data_proc_list(),
  SelfPid = self(),
  PidList =
    lists:map(
      fun(ProcName) ->
        spawn(fun() ->
          Ret = cache_data:execute(ProcName, 0, data_transfer, []),
          SelfPid ! {self(), Ret}
        end)
      end, DataProcNameList),
  lists:foreach(
    fun(Pid) ->
      receive
        {Pid, success} -> success;
        {Pid, {fail, _}} ->
          ?FILE_LOG_ERROR("save player data fail", []),
          fail
      end
    end, PidList).


