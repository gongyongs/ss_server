%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 七月 2014 下午5:55
%%%-------------------------------------------------------------------
-module(dd_config).
-author("zqlt").

-behaviour(gen_server).

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
  reload_cfg/0,
  write_cfg/2,
  get_cfg/1
]).



-define(SERVER, ?MODULE).

-record(state, {path}).
-record(cfg, {key, value}).

%%%===================================================================
%%% API
%%%===================================================================
reload_cfg() ->
  gen_server:call(?MODULE,reload_cfg).

write_cfg(Key, Value) ->
  gen_server:cast(?MODULE, {write_cfg, {Key, Value}}).

get_cfg(Key) ->
  case ets:lookup(cfg, Key) of
    [#cfg{value = Value}] -> {success, Value};
    _ -> fail
  end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(CfgPath::string()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(CfgPath) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [CfgPath], []).

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
init([CfgPath]) ->
  ets:new(cfg, [set, protected, named_table, {keypos, #cfg.key}]),
  load_cfg(CfgPath),
  {ok, #state{path = CfgPath}}.

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
handle_call(reload_cfg, _From,  #state{path = Path} = State) ->
  Ret =
    try
      load_cfg(Path),
      success
    catch
      What:Type ->
        io:format("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
        fail
    end,
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
handle_cast({write_cfg, {Key, Value}}, State) ->
  ets:insert(cfg, #cfg{key = Key, value = Value}),
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
%%加载配置数据到缓存
load_cfg(CfgPath) ->
  {success, Mode} = dd_ms:read_config(mode),
  ParamData = file:consult(CfgPath),
  {ok, [{config_data, ConfigDataList}]} = ParamData,
  lists:foreach(
  fun(ConfigData) ->
    case ConfigData of
      {Mode, ParamList} ->
        lists:foreach(
        fun({Key, Value}) ->
          ets:insert(cfg, #cfg{key= Key, value = Value})
        end, ParamList);
      _ -> ok
    end
  end, ConfigDataList).