%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 七月 2014 下午9:03
%%%-------------------------------------------------------------------
-module(login_cache).
-author("zqlt").

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
-include("login.hrl").
%% API
-export([start_link/0]).
-define(POOL_SIZE, 50).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([
  device_work_proc/0,
  check_device/1,
  bond_device/5
]).

-define(SERVER, ?MODULE).

-record(state, {device_work_pool}).

%%%===================================================================
%%% API
%%%===================================================================
%%register(UName, Password, Device) when is_list(UName) andalso is_list(Password) andalso is_list(Device)->
%%  gen_server:call(?MODULE, {device_request, {Device, {register, {UName, Password, Device}}}}).

%%get_uin_by_uname(Plat, Device, PlayerID, PlayerDisName, PlayerFriend) ->
%%  gen_server:call(?MODULE, {device_request, {Device, {login_by_name, {Plat, Device, PlayerID, PlayerDisName, PlayerFriend}}}}).

%%get_uin_by_device(Device) ->
%%  gen_server:call(?MODULE, {device_request, {Device, {login_by_device, Device}}}).

check_device(Device) ->
  gen_server:call(?MODULE, {event, {Device, {check_device, Device}}}).

bond_device(Plat, Device, PlayerID, PlayerDisName, PlayerFriend) ->
  gen_server:call(?MODULE, {event, {Device, {bond_device, {Plat, Device, PlayerID, PlayerDisName, PlayerFriend}}}}).
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
  DevicePool = create_pool(?POOL_SIZE, device_work_proc, []),
  {ok, #state{device_work_pool = DevicePool}}.

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
handle_call({event, {Device, Event}}, From, #state{device_work_pool = WorkPool} = State) ->
  Work = get_pool_work(Device, WorkPool),
  Work ! {event, {From, Event}},
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
create_pool(PoolCnt, FunName, Param) ->
   TreePool =
    lists:foldr(
      fun(Index, TmpTree) ->
        Pid = spawn(?MODULE, FunName, Param),
        gb_trees:insert(Index, Pid, TmpTree)
      end, gb_trees:empty(), lists:seq(0, PoolCnt - 1)),
  {gb_trees:balance(TreePool), PoolCnt}.

get_pool_work(Key, {PoolTree, PoolSize}) when is_list(Key) ->
  HashValue = hash_service:hash_string(Key),
  RemValue = HashValue rem PoolSize,
  gb_trees:get(RemValue, PoolTree).

get_random_work({PoolTree, PoolSize}) ->
  RandV = dd_util:timestamp() rem PoolSize,
  gb_trees:get(RandV, PoolTree).

%%uname_work_proc() ->
%%  Id = ets:new(userinfo, [set,protected, {keypos, #user_info.uname}]),
%%  put(table_id, Id),
%%  put(mode, uname),
%%  work_proc().

device_work_proc() ->
  Id = ets:new(userinfo, [set,protected, {keypos, #user_info.uin}]),
  put(table_id, Id),
  work_proc().

work_proc() ->
  receive
    {event, {From, {FunName, FunParam}}} ->
      RetValue =
        try
          login_impl:execute(FunName, FunParam)
        catch
          throw:{custom, Reason} ->
            ?FILE_LOG_WARNING("custom reason=~p", [Reason]),
            {fail, Reason};
          What:Type ->
            ?FILE_LOG_ERROR(
              "what=~p, type=~p, stack=~p.",
              [What, Type, erlang:get_stacktrace()]
            ),
            {fail, "HintSystemError"}
        end,
      gen_server:reply(From, RetValue),
      work_proc();
    Other ->
      ?FILE_LOG_ERROR("other msg[~p]", [Other]),
      work_proc()
  end.


get_database_node() ->
  case ets:lookup(login_cfg, database_node) of
    [] ->
      ?FILE_LOG_DEBUG("database node not exist", []),
      throw({custom, "HintSystemError"});
    [#login_cfg{value = Node}] -> Node
  end.
