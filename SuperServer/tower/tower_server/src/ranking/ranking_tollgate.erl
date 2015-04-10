%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十二月 2014 下午4:53
%%%-------------------------------------------------------------------
-module(ranking_tollgate).
-author("zqlt").

-behaviour(gen_server).
-include("ranking.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-include("../cache/cache_def.hrl").
-include("../dd_ms.hrl").
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
  get_friend_tollgate_rank/3,
  get_friend_endless_rank/2,
  get_endless_rank/2
]).

-export([
  update_tollgate_score/3,
  update_endless_score/2,
  update_player_basic_info/2
]).

-export([
  reload_score/0
]).
-define(SERVER, ?MODULE).


-record(state, {load_data_ts}).

%%%===================================================================
%%% API
%%%===================================================================
get_friend_tollgate_rank(Uin, FriendUinList, TollgateID) when is_integer(Uin) andalso is_list(FriendUinList) andalso is_integer(TollgateID) ->
  gen_server:call(?MODULE, {event, {get_friend_tollgate_rank, {Uin, FriendUinList, TollgateID}}}).

get_friend_endless_rank(Uin, FriendUinList) when is_integer(Uin) andalso is_list(FriendUinList) ->
  gen_server:call(?MODULE, {event, {get_friend_endless_rank, {Uin, FriendUinList}}}).

get_endless_rank(Uin, FriendUinList) when is_integer(Uin) andalso is_list(FriendUinList) ->
  gen_server:call(?MODULE, {event, {get_endless_rank, {Uin, FriendUinList}}}).


%%%===================================================================
%%% API
%%%===================================================================
update_tollgate_score(Uin, TollgateID, Score) when is_integer(Uin) andalso is_integer(TollgateID) andalso is_integer(Score) ->
  gen_server:cast(?MODULE, {event, {update_tollgate_score, {Uin, TollgateID, Score}}}).

update_endless_score(Uin, Score) when is_integer(Uin) andalso is_integer(Score) ->
  gen_server:cast(?MODULE, {event, {update_endless_score, {Uin, Score}}}).

update_player_basic_info(Uin, {UName, DisName}) when is_integer(Uin) andalso is_list(DisName) ->
  gen_server:call(?MODULE, {event, {update_player_basic_info, {Uin, UName, DisName}}}).
%%%===================================================================
%%% API
%%%===================================================================
reload_score() ->
  gen_server:call(?MODULE, reload_score).

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
  ets:new(ranking_tollgate, [set, named_table, protected, {keypos, #ranking_tollgate_info.uin}]),
  erlang:start_timer(1*30000, self(), 'start_load_data'),
  {ok, #state{load_data_ts = 0}}.

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
handle_call(reload_score, _From, State) ->
  Ret =
    try
      ranking_tollgate_impl:load_tollgate_data_from_db(),
      success
    catch
      What:Type ->
        ?FILE_LOG_ERROR("load_data from db error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
        fail
    end,
  case Ret of
    success ->  {reply, Ret, State#state{load_data_ts = dd_util:timestamp()}};
    fail -> {reply, Ret, State}
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
handle_cast({event, {FuncName, FuncParam}}, State) ->
  RetValue = work_proc({execute, {FuncName, FuncParam}}),
  ?FILE_LOG_DEBUG("cast: ~p, ~p, result = ~p", [FuncName, FuncParam, RetValue]),
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
handle_info({timeout, _TimerRef, 'start_load_data'}, #state{load_data_ts = LastLoadTs} = State) ->
  Ret =
    try
      if
        LastLoadTs =:= 0 ->
          ranking_tollgate_impl:load_tollgate_data_from_db();
         true -> success
      end
    catch
      What:Type ->
        ?FILE_LOG_ERROR("load_data from db error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
        fail
    end,
  case Ret of
    success ->
      %%通知排名模块，更新排名信息
      ranking_endless:start_ranking_endless(),

      {noreply, State#state{load_data_ts = dd_util:timestamp()}};
    fail ->
      erlang:start_timer(10*60000, self(), 'start_load_data'),
      {noreply, State#state{load_data_ts = 0}}
  end;
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
        ranking_tollgate_impl:execute(FuncName, FuncParam)
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


