%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 十二月 2014 下午1:21
%%%-------------------------------------------------------------------
-module(cache_data).
-author("zqlt").

-behaviour(gen_server).

-include("../../deps/file_log/include/file_log.hrl").
-include("cache_data.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([
  execute/4
]).

-define(SERVER, ?MODULE).

-define(TIMER_SAVE_DATA, 5*60*1000).
-define(TIMER_CLEAN_DATA, 30*60*1000).

-record(state, {
  table_id
}).

%%%===================================================================
%%% API
%%%===================================================================
execute(ProcName, Uin, FuncName, FuncParam) ->
  gen_server:call(ProcName, {event, {Uin, {FuncName, FuncParam}}}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ProcName::term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ProcName) ->
  gen_server:start_link({local, ProcName}, ?MODULE, [], []).

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
  TableID = ets:new(user_info, [set, protected, {keypos, #cache_account.uin}]),
  erlang:start_timer(?TIMER_SAVE_DATA,  self(), 'timer_save_data'),
  erlang:start_timer(?TIMER_CLEAN_DATA, self(), 'timer_clean_data'),
  {ok, #state{table_id = TableID}}.

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
handle_call({event, {Uin, {FuncName, FuncParam}}}, _From, #state{table_id = TableID} = State) ->
  ?FILE_LOG_DEBUG("cache_data: uin = ~p, proc = ~p", [Uin, FuncName]),
  Ret = work_proc(TableID, {event, {FuncName, FuncParam}}),
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
handle_info({timeout, _TimerRef, 'timer_save_data'}, #state{table_id = TableID} = State) ->
  cache_data_proc:timer_save_player(TableID),
  erlang:start_timer(?TIMER_SAVE_DATA,  self(), 'timer_save_data'),
  {noreply, State};
handle_info({timeout, _TimerRef, 'timer_clean_data'}, #state{table_id = TableID} = State) ->
  cache_data_proc:clean_expire_data(TableID),
  erlang:start_timer(?TIMER_CLEAN_DATA, self(), 'timer_clean_data'),
  {noreply, State};
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
terminate(Reason, #state{table_id = TableID} = _State) ->
  ?FILE_LOG_ERROR("cache_data server error, reason = ~p", [Reason]),
  try
    cache_data_proc:save_player_data(TableID),
    ?FILE_LOG_DEBUG("terminate, save player data success", [])
  catch
    What:Type ->
      ?FILE_LOG_ERROR("terminate, save player data error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()])
  end,
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
work_proc(TableID, Event) ->
  case Event of
    {event, {Func, FuncParam}} ->
      try
        cache_data_proc:execute(TableID, Func, FuncParam)
      catch
        throw:{custom, Reason} ->
          ?FILE_LOG_ERROR("cache_data work func ~p error, reason = ~p", [Func, Reason]),
          {fail, Reason};
        What:Type ->
          ?FILE_LOG_ERROR("cache_data proc error => what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
          {fail, "HintSystemError"}
      end;
    Other ->
      ?FILE_LOG_ERROR("cache_data proc error => eror request", [Other]),
      {fail, "HintSystemError"}
  end.