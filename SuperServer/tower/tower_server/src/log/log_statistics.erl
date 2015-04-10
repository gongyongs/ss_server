%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十月 2014 下午8:25
%%%-------------------------------------------------------------------
-module(log_statistics).
-author("zqlt").

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
%% API
-export([start_link/0]).

-include("./log.hrl").
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([
  check_stat/2,
  modify_log_cycle/3,
  reload_log_statistics/0,
  manual_stat/1,
  manual_stat_impl/3
]).

-define(SERVER, ?MODULE).

%%type: day(1-30), hour(1-59), minute(1-59), week(1-3), month(1-11), year(1-100)

-define(STAT_LIST, [
  {"user_retained", {day, 1}},    %%用户留存
%%  {"user_login", {day, 1}},
%%  {"pay_user_retained", {day, 1}},
%%  {"pay_user", {day, 1}},
  {"endless_tollgate", {day, 1}},
  %%{"lottery", {day, 1}},
 %% {"new_player_mission", {day, 1}},
%%  {"pay_rank", {day, 1}},
  {"power_rank", {day, 1}},
  {"shop", {day,1}},
  %%{"strengthen", {day,1}},
  {"tollgate",{day,1}},
  {"tollgate_drain",{day,1}},
  %%{"user_data",{day,1}},
  %%{"reward", {day,1}},
  {"tower_equip",{day,1}},
  {"material_output",{day,1}}
]).

-record(statistics_item, {
  name::string(),
  stat_interval,      %%分钟
  last_ts::integer()
}).
-record(state, {}).

%%%===================================================================
%%% API
%%%================================================================
modify_log_cycle(LogType, CycleType, CycleCount) ->
  gen_server:call(?MODULE, {modify_cycle, {LogType, CycleType, CycleCount}}).

reload_log_statistics() ->
  gen_server:call(?MODULE, reload_log_statistics).

%%2015/1/10
manual_stat(Date) ->
  gen_server:call(?MODULE, {manual_stat, Date}).
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
  ets:new(statistics_lists, [set, protected, named_table, {keypos, #statistics_item.name}]),
  init_statistics_items(),
  erlang:start_timer(1*10000, self(), 'check_statistics'),      %%60000
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
handle_call({modify_cycle, {LogType, CycleType, CycleCount}}, _From, State) ->
  modify_cycle(LogType, CycleType, CycleCount),
  {reply, success, State};
handle_call(reload_log_statistics, _From, State) ->
  reload_statistics_items(),
  {reply, success, State};
handle_call({manual_stat, DateStr}, _From, State) ->
  try
    [Year, Month, Day] = string:tokens(DateStr, "/"),
    Date = {dd_util:to_integer(Year), dd_util:to_integer(Month), dd_util:to_integer(Day)},
    StatList = ets:tab2list(statistics_lists),
    Self = self(),
    lists:map(
      fun(Item) ->
        spawn(?MODULE, manual_stat_impl, [Item, Self, Date])
      end, StatList),
    {reply, success, State}
  catch
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      {reply, fail, State}
  end;
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
handle_info({timeout, _TimerRef, 'check_statistics'}, State) ->
  StatList = ets:tab2list(statistics_lists),
  Self = self(),
  lists:map(
    fun(Item) ->
      spawn(?MODULE, check_stat, [Item, Self])
    end, StatList),
  erlang:start_timer(5*60000, self(), 'check_statistics'),
  {noreply, State#state{}};
handle_info({update_stat_ts, {Type, Ts}}, State) ->
  case ets:update_element(statistics_lists, Type, {#statistics_item.last_ts, Ts}) of
    true ->
      ?FILE_LOG_DEBUG("update stat ts success", []),
      ok;
    Other ->
      ?FILE_LOG_DEBUG("update element error, reason = ~p", [Other])
  end,
  {noreply, State#state{}};
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
reload_statistics_items() ->
  init_statistics_items().

init_statistics_items() ->
  lists:map(
    fun({Name, Cycle}) ->
      NCycle = regular_cycle(Cycle),
      ets:insert(statistics_lists, #statistics_item{name = Name, last_ts = dd_util:timestamp(), stat_interval = NCycle})
    end, ?STAT_LIST).

manual_stat_impl(StatItem, _From, StatDate) ->
  ModName = "stat_" ++ dd_util:to_list(StatItem#statistics_item.name),
  Mod = list_to_atom(ModName),
  try
    case Mod:do_stat(StatItem#statistics_item.stat_interval, StatDate) of
      {success, _UpdateTs} ->
        ?FILE_LOG_DEBUG("~p stat success", [ModName]);
      fail ->
        ?FILE_LOG_DEBUG("stat ~p error", [ModName])
    end
  catch
    {custom, Reason} ->
      ?FILE_LOG_ERROR("do state error, item = ~p, reason = ~p", [StatItem, Reason]);
    What:Type ->
      ?FILE_LOG_ERROR("what=~p,type=~p,stack=~p", [What,Type,erlang:get_stacktrace()])
  end.

check_stat(StatItem, From) when is_record(StatItem, statistics_item) andalso is_pid(From) ->
  ModName = "stat_" ++ dd_util:to_list(StatItem#statistics_item.name),
  Mod = list_to_atom(ModName),
  {Date, _Time} = calendar:local_time(),
  case check_stat_1(StatItem#statistics_item.stat_interval, StatItem#statistics_item.last_ts) of
    true ->
      try
        case Mod:do_stat(StatItem#statistics_item.stat_interval, dd_util:get_last_day(Date)) of
          {success, UpdateTs} ->
            From ! {update_stat_ts, {StatItem#statistics_item.name, UpdateTs}};
          fail ->
            ?FILE_LOG_DEBUG("stat ~p error", [ModName])
        end
      catch
        {custom, Reason} ->
          ?FILE_LOG_ERROR("do state error, item = ~p, reason = ~p", [StatItem, Reason]);
        What:Type ->
          ?FILE_LOG_ERROR("what=~p,type=~p,stack=~p", [What,Type,erlang:get_stacktrace()])
      end;
    false -> ok
  end.

%%检查是否应该统计输出
check_stat_1(_, 0) -> true;
check_stat_1({Type, Count}, LastStatTs) when is_integer(LastStatTs) ->
  CurTime = dd_util:timestamp(),
  Dif = CurTime - LastStatTs,
  case Type of
    minute -> check_stat_2(dif, {Dif, Count});
    hour -> check_stat_2(dif, {Dif, Count});
    _ -> check_stat_2(day, {LastStatTs, Count})
  end.



%%每天都统计
check_stat_2(dif, {Dif, Cycle}) ->
  if
    Dif > Cycle -> true;
    true -> false
  end;
check_stat_2(day, {LastTs, _Cycle}) ->
  if
    LastTs =:= 0 -> true;
    true ->
      {CD, CT} = calendar:local_time(),
      {LD, LT} = dd_util:to_local_time(dd_util:timestamp_to_datetime(LastTs)),
      case dd_util:time_compare_by_datetime({CD, CT}, {CD, ?LOG_STAT_TIME}) of
        -1 -> false;
        _ ->
          case dd_util:time_compare_by_datetime({LD, LT}, {CD, ?LOG_STAT_TIME}) of
            -1 -> true;
            _ -> false
          end
      end
  end.

modify_cycle(Type, CycleType, CycleVal) when is_list(Type) andalso is_atom(CycleType) andalso is_integer(CycleVal) ->
  case ets:lookup(statistics_lists, Type) of
    [] ->
      ?FILE_LOG_DEBUG("not exist type, type = ~p", [Type]),
      ets:insert(statistics_lists, #statistics_item{name = Type, last_ts = 0, stat_interval = {CycleType, CycleVal}}),
      success;
    [OldCycle] ->
      NCycle = regular_cycle({CycleType, CycleVal}),
      ets:insert(statistics_lists, OldCycle#statistics_item{stat_interval = NCycle}),
      success
  end.

regular_cycle(Cycle) ->
  case Cycle of
    {year, N} ->
      if
        N < 1 -> {year, 1};
        true -> {year, N}
      end;
    {month, N} ->
      if
        N < 1 -> {month, 1};
        N > 11 -> {month, 11};
        true -> {month, N}
      end;
    {week, N} ->
      if
        N < 1 -> {week, 1};
        N > 3 -> {week, 3};
        true -> {week, N}
      end;
    {day, N} ->
      if
        N < 1 -> {day, 1};
        N > 30 -> {day, 30};
        true -> {day, N}
      end;
    {hour, N} ->
      if
        N < 1 -> {hour, 1};
        N > 23 -> {hour, 23};
        true -> {hour, N}
      end;
    {minute, N} ->
      if
        N < 1 -> {minute, 1};
        N > 59 -> {minute, 59};
        true -> {minute, N}
      end;
    _ ->
      ?FILE_LOG_DEBUG("error cycle", []),
      {day, 1}
  end.