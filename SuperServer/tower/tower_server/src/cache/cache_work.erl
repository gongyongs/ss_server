%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 七月 2014 上午10:18
%%% 每一个玩家开一个进程处理事务
%%%-------------------------------------------------------------------
-module(cache_work).
-author("zqlt").

-behaviour(gen_server).

-include("../../deps/file_log/include/file_log.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([execute/4,
  execute_func/4]).

-define(SERVER, ?MODULE).
-define(POOL_SIZE, 5).
-record(state, {work_tree}).

%%%===================================================================
%%% API
%%%===================================================================
execute(ProcName, Uin, FuncName, FuncParam) ->
  gen_server:call(ProcName, {execute, Uin, {FuncName, FuncParam}}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ProcName::atom()) ->
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
  {success, RankNode} = dd_ms:read_config(ranking_node),
  dd_config:write_cfg(ranking_node, RankNode),
  {ok, #state{work_tree = gb_trees:empty()}}.

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
handle_call({execute, Uin, {FuncName, FuncParam}}, From, #state{work_tree = WorkTree} = State) ->
  Self = self(),
  NewWorkTree =
    case gb_trees:lookup(Uin, WorkTree) of
      none ->
        ?FILE_LOG_DEBUG("execute [~p]", [Uin]),
        spawn(?MODULE, execute_func, [Self, From, Uin, {FuncName, FuncParam}]),
        gb_trees:insert(Uin, [], WorkTree);
      {value, EventList} ->
        ?FILE_LOG_DEBUG("add event to queue [~p]", [Uin]),
        gb_trees:update(Uin, [{FuncName, FuncParam, From} | EventList], WorkTree)
    end,
  {noreply, State#state{work_tree = NewWorkTree}};
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
handle_info({event_complete, Uin}, #state{work_tree = WorkTree} = State) ->
  ?FILE_LOG_DEBUG("event complete [~p]", [Uin]),
  Self = self(),
  NewTree =
    case gb_trees:lookup(Uin, WorkTree) of
      none ->
        ?FILE_LOG_ERROR("work tree exception [~p]", [Uin]),
        WorkTree;
      {value, []} ->
        ?FILE_LOG_DEBUG("event queue empty [~p]", [Uin]),
        gb_trees:delete(Uin, WorkTree);
      {value, EventList} ->
        ?FILE_LOG_DEBUG("get next event [~p]", [Uin]),
        [{FuncName, FuncParam, From} | T] = lists:reverse(EventList),
        spawn(?MODULE, execute_func, [Self, From, Uin, {FuncName, FuncParam}]),
        gb_trees:update(Uin, lists:reverse(T), WorkTree)
    end,
  {noreply, State#state{work_tree = NewTree}};
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
terminate(Reason, _State) ->
  ?FILE_LOG_INFO("cache_work terminate, reason = ~p", [Reason]).

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
execute_func(Self, From, Uin, {FuncName, FuncParam}) ->
  RetValue =
    try
      ?FILE_LOG_DEBUG("cache_work: execute [~p] [~p]", [FuncName, FuncParam]),
      cache_work_proc:execute(FuncName, FuncParam)
    catch
      throw:{custom, Reason} -> {fail, Reason};
      What:Type ->
        ?FILE_LOG_ERROR("What = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
        {fail, "HintSystemError"}
    end,

  %%回复消息
  gen_server:reply(From, RetValue),

  %%处理下一个事件
  Self ! {event_complete, Uin}.