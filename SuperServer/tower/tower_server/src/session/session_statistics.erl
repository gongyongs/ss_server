%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 十月 2014 下午2:29
%%%-------------------------------------------------------------------
-module(session_statistics).
-author("zqlt").

-behaviour(gen_server).
-include("session.hrl").
%% API
-export([start_link/0]).

-include("../../deps/file_log/include/file_log.hrl").
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([
  add_operation_time/3,
  next_day_clear/0,
  operation_time_log/1,
  operation_time_log_proc/2,
  login/4,
  output_operation_log/0
]).
-define(SERVER, ?MODULE).

-define(EXPIRE_TIME, 20*60).

-record(user_operation_statistics, {uin, operation_time, ip, device}).
-record(state, {}).
%%统计一天的累计游戏时长
%%%===================================================================
%%% API
%%%===================================================================
output_operation_log() ->
  gen_server:call(?MODULE, out_put_operation_log).

login(Uin, OperationTime, Ip, Device) ->
  gen_server:cast(?MODULE, {login_to_update_time, {Uin, OperationTime, Ip, Device}}).

add_operation_time(Uin, Time, RequestType) ->
  gen_server:cast(?MODULE, {refresh_time, {Uin, Time, RequestType}}).

next_day_clear() ->
  gen_server:cast(?MODULE, clear_time).

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
  ets:new(operation_statistics, [set, protected, named_table, {keypos, #user_operation_statistics.uin}]),
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
handle_call(out_put_operation_log, _From, State) ->
  try
    output_log()
  catch
    What:Type ->
      ?FILE_LOG_DEBUG("what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()])
  end,
  {reply, ok, State};
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
handle_cast({login_to_update_time,  {Uin, OperationTime, Ip, Device}}, State) ->
  case ets:lookup(operation_statistics, Uin) of
    [] ->
      ets:insert(operation_statistics, #user_operation_statistics{uin = Uin, operation_time = OperationTime, ip = Ip, device = Device});
    [UserOperationInfo] ->
      NT = UserOperationInfo#user_operation_statistics.operation_time + OperationTime,
      ets:insert(operation_statistics, UserOperationInfo#user_operation_statistics{operation_time = NT})
  end,
  {noreply, State};
handle_cast({refresh_time, {Uin, TimeAdd, RequestType}}, State) ->
  Dif =
    case RequestType of
      {game_end, TotalKillMonster} ->
        (TotalKillMonster div 10) * 60;
      {endless_game_end, Wave} ->
        (Wave - 1)*2*60;
      _Other ->
        if
          TimeAdd > 2*60 -> 2*60;
          true -> TimeAdd
        end
    end,
    case ets:lookup(operation_statistics, Uin) of
      [] ->
        ets:insert(operation_statistics, #user_operation_statistics{uin = Uin, operation_time = Dif, ip = "", device = ""});
      [UserOperationInfo] ->
        NT = UserOperationInfo#user_operation_statistics.operation_time + Dif,
        ets:insert(operation_statistics, UserOperationInfo#user_operation_statistics{operation_time = NT})
    end,
  {noreply, State};
handle_cast(clear_time, State) ->
  ?FILE_LOG_DEBUG("clear operation time", []),
  List = ets:tab2list(operation_statistics),
  ?FILE_LOG_DEBUG("operation_statistics: list = ~p", [List]),
  ets:delete_all_objects(operation_statistics),
  spawn(?MODULE, operation_time_log, [List]),
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
operation_time_log(List) ->
  ?FILE_LOG_DEBUG("delete operation_statistics", []),
  {success, [CacheNode | _]} = dd_ms:get_all_cache(),
  NList = segment_list(List),
  lists:foreach(
    fun(SubList) ->
      spawn(?MODULE, operation_time_log_proc, [SubList, CacheNode])
    end, NList).

operation_time_log_proc(List, Node) when is_list(List) and is_atom(Node) ->
  Now = dd_util:time_format(),
  lists:foreach(
    fun(Item) ->
      case catch rpc:call(Node, cache_log, log_operation_time, [Item#user_operation_statistics.uin, Now, Item#user_operation_statistics.operation_time]) of
        ok -> ok;
        {badrpc, Reason} ->
          ?FILE_LOG_ERROR("cache_opration_log_time; error, reason = ~p, uin = ~p, operation_time = ~p", [Reason, Item#user_operation_statistics.uin, Item#user_operation_statistics.operation_time]);
        Other ->
          ?FILE_LOG_ERROR("error =~p , stack = ~p", [Other, erlang:get_stacktrace()])
      end
    end, List).

segment_list(List) ->
  segment_list_1(List, []).
segment_list_1([], OutList) -> OutList;
segment_list_1(List, OutList) when length(List) < 100 ->
  {L1, L2} = lists:split(100, List),
  segment_list_1(L2, [L1 | OutList]).

output_log() ->
  Path = os:getenv("STATISTICS_PATH"),
  {Date, _} = calendar:local_time(),
  FileName = dd_util:to_list(Path) ++ dd_util:time_format_without_hms(Date) ++ ".txt",
  ?FILE_LOG_DEBUG("filename = ~p", [FileName]),
  {ok, F} = file:open(FileName, write),
  List = ets:tab2list(operation_statistics),
  lists:foreach(
    fun(Item) ->
      V = lists:flatten(io_lib:format("~p,~p,~p,~p~n", [Item#user_operation_statistics.ip, Item#user_operation_statistics.device, Item#user_operation_statistics.uin, Item#user_operation_statistics.operation_time div 60])),
      ?FILE_LOG_DEBUG("value = ~p", [V]),
      file:write(F, dd_util:to_binary(V))
    end, List),
  file:close(F).



