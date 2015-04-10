%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 十二月 2014 下午2:23
%%%-------------------------------------------------------------------
-module(concurrency_test).
-author("zqlt").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-include("concurrency_test.hrl").
-include("../../deps/file_log/include/file_log.hrl").
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([
  cast_request_info/1,
  out_put_result/0,
  cast_result/2
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
cast_result(Result, RequestRd) ->
  NRd = RequestRd#request_rd{request_over_tms = dd_util:milliseconds(), request_cost = dd_util:milliseconds() - RequestRd#request_rd.request_start_tms, response_result = Result},
  concurrency_test:cast_request_info(NRd).

cast_request_info(ReqInfo) when is_record(ReqInfo, request_rd) ->
  ?FILE_LOG_DEBUG("CAST request info, ~p", [ReqInfo]),
  gen_server:cast(?MODULE, {requst_info, ReqInfo}).

out_put_result() ->
  gen_server:call(?MODULE, out_put_result).

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
  ets:new(concurrency_rd, [set, protected, named_table, {keypos, #request_info.request_id}]),
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
handle_call(out_put_result, _From, State) ->
  output(),
  {reply, success, State};
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
handle_cast({requst_info, ReqInfo}, State) ->
  ?FILE_LOG_DEBUG("write concurrent log = ~p", [ReqInfo]),
  case ets:lookup(concurrency_rd, ReqInfo#request_rd.request_id) of
    [] ->
      Req = #request_info{request_id = ReqInfo#request_rd.request_id, request_list = [ReqInfo]},
      Sta = ets:insert(concurrency_rd, Req),
      ?FILE_LOG_DEBUG("insert = ~p", [Sta]);
    [RequestInfo] ->
      Req = RequestInfo#request_info{request_list = [ReqInfo | RequestInfo#request_info.request_list]},
      St1 = ets:insert(concurrency_rd, Req),
      ?FILE_LOG_DEBUG("insert = ~p", [St1])
  end,
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
terminate(Reason, _State) ->
  ?FILE_LOG_DEBUG("terminate! reason = ~p", [Reason]),
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
output() ->
  Path = os:getenv("SASL_LOG_PATH") ++ "/",
  ?FILE_LOG_DEBUG("concurrency test path = ~p", [Path]),
  Time = dd_util:time_format(),
  lists:map(
    fun(RequestInfo) ->
      FileName = Path ++ dd_util:to_list(RequestInfo#request_info.request_id) ++ "_" ++ Time ++ ".csv",
      ?FILE_LOG_DEBUG("concurrency file name = ~p", [FileName]),
      {ok, F} = file:open(FileName, [write]),
      io:format(F, "~s,~s,~s,~s~n", ["RequestID", "RequestTime", "RequestCost", "Result"]),
      {{SuccessCnt, TotalSuccessCost}, {FailCnt, TotalFailCost}} =
        lists:foldl(
          fun(RequestRD, {{TmpSuccessCnt, TmpTotalSuccessCost}, {TmpFailCnt, TmpFailCost}}) ->
            io:format(F, "~p,~p,~p,~p~n", [RequestRD#request_rd.request_id, RequestRD#request_rd.request_time, RequestRD#request_rd.request_cost, RequestRD#request_rd.response_result]),
            case RequestRD#request_rd.response_result of
              0 -> {{TmpSuccessCnt + 1, TmpTotalSuccessCost + RequestRD#request_rd.request_cost}, {TmpFailCnt, TmpFailCost}};
              _ -> {{TmpSuccessCnt, TmpTotalSuccessCost}, {TmpFailCnt + 1, TmpFailCost + RequestRD#request_rd.request_cost}}
            end
          end, {{0, 0}, {0, 0}}, RequestInfo#request_info.request_list),
      AveSuccess =
        if
          SuccessCnt =:= 0 -> 0;
          true ->
            TotalSuccessCost div SuccessCnt
        end,
      AveFail =
        if
          FailCnt =:= 0 -> 0;
          true ->
            TotalFailCost div FailCnt
        end,
      ?FILE_LOG_DEBUG("total_cnt = ~p,success_cnt = ~p,fail_cnt = ~p, ave_success = ~p, ave_fail = ~p", [SuccessCnt + FailCnt, SuccessCnt, FailCnt, AveSuccess, AveFail]),
      io:format(F, "~ntotal_cnt = ~p,success_cnt = ~p,fail_cnt = ~p, ave_success = ~p, ave_fail = ~p", [SuccessCnt + FailCnt, SuccessCnt, FailCnt, AveSuccess, AveFail]),
      file:close(F)
    end, ets:tab2list(concurrency_rd)).
