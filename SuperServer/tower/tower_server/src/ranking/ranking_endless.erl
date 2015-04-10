%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十二月 2014 下午9:02
%%%-------------------------------------------------------------------
-module(ranking_endless).
-author("zqlt").

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
-include("ranking.hrl").
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
  start_ranking_endless/0,
  get_endless_rank/1,
  update_player_basic_info/2
]).

-export([
  settle_weekly_rank/1
]).

-define(SERVER, ?MODULE).

-record(state, {
  last_rank_ts,    %%上一次排名的时间
  last_settle_ts   %%上一次结算的时间
}).

%%%===================================================================
%%% API
%%%===================================================================
start_ranking_endless() ->
  gen_server:cast(?MODULE, start_ranking_endless).

get_endless_rank(Uin) ->
  gen_server:call(?MODULE, {event, {get_endless_rank, Uin}}).

update_player_basic_info(Uin, {UName, DisName}) ->
  gen_server:call(?MODULE, {event, {update_player_basic_info, {Uin, UName, DisName}}}).
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
  ets:new(endless_server_rank, [set, protected, named_table, {keypos, #rank_info.uin}]),
  ets:new(endless_server_rank_top_20, [set, protected, named_table, {keypos, #rank_info.uin}]),
  erlang:start_timer(5 * 60 * 1000, self(), 'check_ranking'),
  erlang:start_timer(10*60*1000, self(), 'check_rank_settlement'),
  {ok, #state{last_rank_ts = 0, last_settle_ts = 0}}.

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
handle_call({event, {FuncName, FuncParam}}, _From, State) ->
  RetValue = work_proc({execute, {FuncName, FuncParam}}),
  {reply, RetValue, State};
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
handle_cast(start_ranking_endless, #state{last_rank_ts = LastRankTs} = State) ->
  ?FILE_LOG_DEBUG("start ranking endless",[]),
  Ts =
    try
      ets:delete_all_objects(endless_server_rank),
      ets:delete_all_objects(endless_server_rank_top_20),
      success = ranking_endless_impl:ranking_endless(),
      dd_util:timestamp()
    catch
      {custom, Reason} ->
        ?FILE_LOG_ERROR("ranking endless error, reason = ~p", [Reason]),
        LastRankTs;
      What:Type ->
        ?FILE_LOG_ERROR("ranking endless error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
        LastRankTs
    end,
  {noreply, State#state{last_rank_ts = Ts}};
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
handle_info({timeout, _TimerRef, 'check_ranking'}, #state{last_rank_ts = LastRankTs} = State) ->
  RankTS =
    try
      if
        LastRankTs =:= 0 -> start_ranking_endless();
        true ->
          case ranking_endless_impl:check_endless_rank(LastRankTs) of
            time_to_rank ->
              start_ranking_endless(),
              dd_util:timestamp();
            not_the_time -> LastRankTs
          end
      end
    catch
      throw:{custom, Reason} ->
        ?FILE_LOG_DEBUG("time out endless rank error, reason = ~p", [Reason]),
        LastRankTs;
      What:Type ->
        ?FILE_LOG_ERROR("time out endless rank error,  what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
        LastRankTs
    end,
  erlang:start_timer(5 * 60 * 1000, self(), 'check_ranking'),
  {noreply, State#state{last_rank_ts = RankTS}};
handle_info({timeout, _TimerRef, 'check_rank_settlement'}, #state{last_settle_ts = LastSettleTs, last_rank_ts = LastRankTs} = State) ->
  NState =
    try
      case ranking_endless_impl:check_endless_settlement(LastRankTs, LastSettleTs) of
        time_to_rank_and_settle ->
          success = ranking_endless_impl:ranking_endless(),
          Self = self(),
          Pid = spawn(?MODULE, settle_weekly_rank, [Self]),
          ?FILE_LOG_DEBUG("time to settle week endless rank, pid = ~p", [Pid]),
          State#state{last_rank_ts = dd_util:timestamp(), last_settle_ts = dd_util:timestamp()};
        time_to_settle ->
          Self = self(),
          Pid = spawn(?MODULE, settle_weekly_rank, [Self]),
          ?FILE_LOG_DEBUG("time to settle week endless rank, pid = ~p", [Pid]),
          State#state{last_settle_ts = dd_util:timestamp()};
        not_the_time ->
          State
      end
    catch
      {custom, Reason} ->
        ?FILE_LOG_ERROR("check_rank_settlement timer error, reason = ~p", [Reason]),
        State;
      What:Type ->
        ?FILE_LOG_ERROR("check_rank_settlement timer error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
        State
    end,
  erlang:start_timer(10*60*1000, self(), 'check_rank_settlement'),
  {noreply, NState};
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
work_proc(Param) ->
  case Param of
    {execute, {FuncName, FuncParam}} ->
      try
        ranking_endless_impl:execute(FuncName, FuncParam)
      catch
        throw:{custom, Reason} ->
          ?FILE_LOG_WARNING("custom reason :~p", [Reason]),
          {fail, Reason};
        What:Type ->
          ?FILE_LOG_ERROR( "what=~p, type=~p, stack=~p", [What, Type, erlang:get_stacktrace()] ),
          {fail, "HintSystemError"}
      end;
    Other ->
      ?FILE_LOG_DEBUG("ranking_work:work_proc receive = [~p]", [Other]),
      {fail, "HintSystemError"}
  end.


settle_weekly_rank(Pid) ->
  ranking_endless_impl:settle_weekly_endless_rank(Pid).




