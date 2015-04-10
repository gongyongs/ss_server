%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 八月 2014 上午10:02
%%%-------------------------------------------------------------------
-module(cache_guid).
-author("zqlt").

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
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
-define(POOL_SIZE, 5).
-export([
  alloc_guid/1,
  get_work_proc/2,
  work_func/0
]).

-record(state, {work_tree}).

%%%===================================================================
%%% API
%%%===================================================================
alloc_guid(Uin) ->
  gen_server:call(?MODULE, {alloc_guid, Uin}).

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
  WorkPool = create_work_pool(?POOL_SIZE, work_func, []),
  {ok, #state{work_tree = WorkPool}}.

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
handle_call({alloc_guid, Uin}, From, #state{work_tree = WorkTree} = State) ->
  Work = get_work_proc(Uin, WorkTree),
  Work ! {alloc_guid, From, Uin},
  {noreply, State};
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
create_work_pool(PoolCnt, FuncName, FuncParam) ->
  WorkTree =
    lists:foldr(
      fun(Index, TmpTree) ->
        Pid = spawn(?MODULE, FuncName, FuncParam),
        gb_trees:insert(Index, Pid, TmpTree)
      end,gb_trees:empty(), lists:seq(1, PoolCnt)),
  {gb_trees:balance(WorkTree), PoolCnt}.

get_work_proc(Uin, {WorkTree, PoolSize}) when is_integer(Uin)->
  RemV = Uin rem PoolSize,
  gb_trees:get(RemV + 1, WorkTree).

work_func() ->
  try
    receive
      {alloc_guid, From, Uin} ->
        {success, DbNode} = dd_config:get_cfg(database_node),
        case rpc:call(DbNode, database, alloc_guid, [Uin]) of
          {success, Guid} ->
            gen_server:reply(From, {success, Guid}),
            work_func();
          {badrpc, Reason} ->
            ?FILE_LOG_ERROR("rpc call database alloc guid error reason: [~p]", [Reason]),
            gen_server:reply(From, fail),
            work_func();
          Other ->
            ?FILE_LOG_ERROR("rpc call database alloc guid other return: [~p]", [Other]),
            gen_server:reply(From, fail),
            work_func()
        end
    end
  catch
    _:_ ->
      work_func()
  end.
