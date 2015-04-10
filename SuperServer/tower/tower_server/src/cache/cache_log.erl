%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 八月 2014 上午10:07
%%%-------------------------------------------------------------------
-module(cache_log).
-author("zqlt").

-behaviour(gen_server).
 -include("../../deps/file_log/include/file_log.hrl").
-include("../../deps/mysql/include/mysql.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-export([
  log_money_flow/5,
  log_game_end_flow/11,
  log_lottery_flow/5,
  log_login/5,
  log_register/7,
  log_reward_flow/7,
  log_success_pay_flow/10,
  log_fail_pay_flow/6,
  log_consume_flow/9,
  log_strengthen_flow/10,
  log_operation_time/3,
  log_mission_flow/5,
  log_achievement_flow/5
]).

-record(state, {log_node}).

%%%===================================================================
%%% API
%%%===================================================================
log_money_flow(Uin, FlowType, MoneyType, Before, After) ->
  gen_server:cast(?MODULE, {log_money_flow, {Uin, FlowType, MoneyType, Before, After}}).

log_game_end_flow(Uin, Time, GameResult, TollgateID, TollgateType, GainGold, GainStar, GainScore, EndlessCount, UsePropStr, GainDropStr) ->
  gen_server:cast(?MODULE, {log_game_end_flow, {Uin, Time, GameResult, TollgateID, TollgateType, GainGold, GainStar, GainScore, EndlessCount, UsePropStr, GainDropStr}}).

log_lottery_flow(Uin, LotteryType, CostType, CostCount, GainDropStr) ->
  gen_server:cast(?MODULE, {log_lottery_flow, {Uin, LotteryType, CostType, CostCount, GainDropStr}}).

log_login(Uin, PlatType, LoginTime, Device, Ip) ->
  gen_server:cast(?MODULE, {log_login_flow, {Uin, PlatType, LoginTime, Device, Ip}}).

log_register(Uin, RegisterTime, PlatType, PlatID, PlatDisName, Device, Ip) ->
  gen_server:cast(?MODULE, {log_register_flow, {Uin, RegisterTime, PlatType, PlatID, PlatDisName, Device, Ip}}).

log_reward_flow(Uin, RewardType, GetRewardTime, RewardItemType, RewardItemID, RewardItemCount, RewardDesc) ->
  gen_server:cast(?MODULE, {log_reward_flow, {Uin, RewardType, GetRewardTime, RewardItemType, RewardItemID, RewardItemCount, RewardDesc}}).

log_success_pay_flow(Uin, Plat, PayOrder, PayTime, PayItemID, BuyItemType, BuyItemID, BuyCount, BuyDesc, BuyBackup) ->
  gen_server:cast(?MODULE, {log_success_pay_flow, {Uin,  Plat, PayOrder, PayTime, PayItemID, BuyItemType, BuyItemID, BuyCount, BuyDesc, BuyBackup}}).

log_fail_pay_flow(Uin,  Plat, PayOrder, PayTime, PayInfo, Reason) ->
  gen_server:cast(?MODULE, {log_fail_pay_flow, {Uin,  Plat, PayOrder, PayTime, PayInfo, Reason}}).

log_consume_flow(Uin, BuyTime, BuyItemType, CommodityID, BuyItemID, BuyItemCount, CostType, CostCount, BuyDesc) ->
  gen_server:cast(?MODULE, {log_consume_flow, {Uin, BuyTime, BuyItemType, CommodityID, BuyItemID, BuyItemCount, CostType, CostCount, BuyDesc}}).

log_strengthen_flow(Uin, StrengthenType, StrengthenObjID, StrengthenObjNo, ConsumeGoldCount, SwallowStr, BeforeObjNo, BeforeObjExp, AfterObjNo, AfterObjExp) ->
  gen_server:cast(?MODULE, {log_strengthen_flow, {Uin, StrengthenType, StrengthenObjID, StrengthenObjNo, ConsumeGoldCount, SwallowStr, BeforeObjNo, BeforeObjExp, AfterObjNo, AfterObjExp}}).
%%date: string
log_operation_time(Uin, Date, Time) ->
  gen_server:cast(?MODULE, {log_operation_time, {Uin, Date, Time}}).

%%mission status: 1 已开始， 2: 已结束
log_mission_flow(Uin, MissionType, MissionID, MissionDesc, MissionStatus) ->
  gen_server:cast(?MODULE, {log_mission_flow, {Uin, MissionType, MissionID, MissionDesc, MissionStatus}}).

log_achievement_flow(Uin, AchievementType, AchievementID, AchievementDesc, AchievementStatus) ->
  gen_server:cast(?MODULE, {log_achievement_flow, {Uin, AchievementType, AchievementID, AchievementDesc, AchievementStatus}}).
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
  {success, LogNode} = dd_ms:read_config(log_node),
  {ok, #state{log_node = LogNode}}.

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
handle_cast({log_money_flow, {Uin, FlowType, MoneyType, Before, After}}, #state{log_node = Node}=State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|MoneyFlow|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",\"",
      dd_util:to_list(FlowType), "\",",
      dd_util:to_list(MoneyType), ",",
      dd_util:to_list(Before), ",",
      dd_util:to_list(After), ",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
	%%?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_game_end_flow, {Uin, Time, GameResult, TollgateID, TollgateType, GainGold, GainStar, GainScore, EndlessCount, UsePropStr, GainDropStr}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|GameEndFlow|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",",
      dd_util:to_list(Time), ",",
      dd_util:to_list(GameResult), ",",
      dd_util:to_list(TollgateID), ",",
      dd_util:to_list(TollgateType), ",",
      dd_util:to_list(GainGold), ",",
      dd_util:to_list(GainStar), ",",
      dd_util:to_list(GainScore), ",",
      dd_util:to_list(EndlessCount), ",\"",
      dd_util:to_list(UsePropStr), "\",\"",
      dd_util:to_list(GainDropStr), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
  %%?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_lottery_flow, {Uin, LotteryType, CostType, CostCount, GainDropStr}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|Lottery|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",",
      dd_util:to_list(LotteryType), ",",
      dd_util:to_list(CostType), ",",
      dd_util:to_list(CostCount), ",\"",
      dd_util:to_list(GainDropStr), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
  %%?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_login_flow, {Uin, PlatType, LoginTime, Device, Ip}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|Login|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",",
      dd_util:to_list(PlatType), ",",
      dd_util:to_list(LoginTime), ",\"",
      dd_util:to_list(Device), "\",\"",
      dd_util:to_list(Ip), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
  %%?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_register_flow, {Uin, RegisterTime, PlatType, PlatID, PlatDisName, Device, Ip}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|Register|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",",
      dd_util:to_list(RegisterTime), ",",
      dd_util:to_list(PlatType), ",\"",
      dd_util:to_list(PlatID), "\",\"",
      dd_util:to_list(PlatDisName), "\",\"",
      dd_util:to_list(Device), "\",\"",
      dd_util:to_list(Ip), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
 %% ?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_reward_flow, {Uin, RewardType, GetRewardTime, RewardItemType, RewardItemID, RewardItemCount, RewardDesc}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|RewardFlow|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",",
      dd_util:to_list(RewardType), ",",
      dd_util:to_list(GetRewardTime), ",",
      dd_util:to_list(RewardItemType), ",\"",
      dd_util:to_list(RewardItemID), "\",",
      dd_util:to_list(RewardItemCount), ",\"",
      dd_util:to_list(RewardDesc), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
 %% ?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_success_pay_flow, {Uin,  Plat, PayOrder, PayTime, PayItemID, BuyItemType, BuyItemID, BuyCount, BuyDesc, BuyBackup}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|PaySuccessFlow|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",\"",
      dd_util:to_list(Plat), "\", \"",
      dd_util:to_list(PayOrder), "\",",
      dd_util:to_list(PayTime), ",\"",
      dd_util:to_list(PayItemID), "\",",
      dd_util:to_list(BuyItemType), ",\"",
      dd_util:to_list(BuyItemID), "\",",
      dd_util:to_list(BuyCount), ",\"",
      dd_util:to_list(BuyDesc), "\",\"",
      dd_util:to_list(BuyBackup), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
  %%?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_fail_pay_flow, {Uin, Plat, PayOrder, PayTime, PayInfo, Reason}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|PayFailFlow|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",\"",
      dd_util:to_list(Plat), "\",\"",
      dd_util:to_list(PayOrder), "\",",
      dd_util:to_list(PayTime), ",\"",
      dd_util:to_list(PayInfo), "\",\"",
      dd_util:to_list(Reason), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
 %% ?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_consume_flow, {Uin, BuyTime, BuyItemType, CommodityID, BuyItemID, BuyItemCount, CostType, CostCount, BuyDesc}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|ConsumeFlow|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",",
      dd_util:to_list(BuyTime), ",",
      dd_util:to_list(BuyItemType), ",\"",
      dd_util:to_list(CommodityID), "\",\"",
      dd_util:to_list(BuyItemID), "\",",
      dd_util:to_list(BuyItemCount), ",",
      dd_util:to_list(CostType), ",",
      dd_util:to_list(CostCount), ",\"",
      dd_util:to_list(BuyDesc),"\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
 %% ?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_strengthen_flow, {Uin, StrengthenType, StrengthenObjID, StrengthenObjNo, ConsumeGoldCount, SwallowStr, BeforeObjNo, BeforeObjExp, AfterObjNo, AfterObjExp}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|StrengthenFlow|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",",
      dd_util:to_list(StrengthenType), ",\"",
      dd_util:to_list(StrengthenObjID), "\",\"",
      dd_util:to_list(StrengthenObjNo), "\",",
      dd_util:to_list(ConsumeGoldCount), ",\"",
      dd_util:to_list(SwallowStr), "\",\"",
      dd_util:to_list(BeforeObjNo), "\",",
      dd_util:to_list(BeforeObjExp), ",\"",
      dd_util:to_list(AfterObjNo), "\",",
      dd_util:to_list(AfterObjExp), ",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
 %% ?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_operation_time, {Uin, Date, Time}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|OperationTimeFlow|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",\"",
      dd_util:to_list(Date), "\",",
      dd_util:to_list(Time), ",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
  %%?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_mission_flow, {Uin, MissionType, MissionID, MissionDesc, MissionStatus}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|MissionFlow|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",",
      dd_util:to_list(MissionType), ",\"",
      dd_util:to_list(MissionID), "\",\"",
      mysql_util:escape(dd_util:to_binary(MissionDesc)), "\",",
      dd_util:to_list(MissionStatus), ",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
  %%?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({log_achievement_flow, {Uin, AchievementType, AchievementID, AchievementDesc, AchievementStatus}}, #state{log_node = Node} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  Format =
    [
      "LogDB|AchievementFlow|\"", Now, "\",\"", Key, "\",",
      dd_util:to_list(Uin), ",",
      dd_util:to_list(AchievementType), ",\"",
      dd_util:to_list(AchievementID), "\",\"",
      mysql_util:escape(dd_util:to_binary(AchievementDesc)), "\",",
      dd_util:to_list(AchievementStatus), ",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(Node, log, write_log, [LogBin]),
  %%?FILE_LOG_DEBUG("log=~p", [LogBin]),
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
