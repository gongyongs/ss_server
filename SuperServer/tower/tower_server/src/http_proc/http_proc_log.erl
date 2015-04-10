%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 十二月 2014 下午2:13
%%%-------------------------------------------------------------------
-module(http_proc_log).
-author("zqlt").

-behaviour(gen_server).

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
  write_pp_pay_log/2,
  write_360_pay_log/2,
  write_ky_pay_log/2,
  write_tb_pay_log/2
]).

-include("../../deps/file_log/include/file_log.hrl").
-define(SERVER, ?MODULE).

-record(state, {log_node}).

%%%===================================================================
%%% API
%%%===================================================================
write_tb_pay_log(TableName, PayRd) ->
  gen_server:cast(?MODULE, {write_tb_pay, {TableName, PayRd}}).

write_ky_pay_log(TableName, PayRd) ->
  gen_server:cast(?MODULE, {write_ky_pay, {TableName, PayRd}}).

write_pp_pay_log(TableName, PayRd) ->
  gen_server:cast(?MODULE, {write_pp_pay, {TableName, PayRd}}).

write_360_pay_log(TableName, PayRd) ->
  gen_server:cast(?MODULE, {write_360_pay, {TableName, PayRd}}).

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
  {success, LogNode} = dd_ms:read_config(log_node),
  {ok, #state{log_node=LogNode}}.

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
handle_cast({write_tb_pay, {TableName, PayRd}},  #state{log_node = LogNode} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  {Uin, OrderID, BillNo, Account, GoodID, GoodPrice, AmountFee, Reason} = PayRd,
  Format =
    [
      "LogDB|", TableName,"|\"",
      dd_util:to_list(Now), "\",\"",
      dd_util:to_list(Key), "\",",
      dd_util:to_list(Uin), ",\"",
      "TB", "\",\"",
      dd_util:to_list(OrderID), "\",\"",
      dd_util:to_list(BillNo), "\",\"",
      dd_util:to_list(Account), "\",\"",
      dd_util:to_list(GoodID), "\",\"",
      dd_util:to_list(GoodPrice), "\",\"",
      dd_util:to_list(AmountFee), "\",\"",
      dd_util:to_list(Reason), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(LogNode, log, write_log, [LogBin]),
  ?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({write_ky_pay, {TableName, PayRd}},  #state{log_node = LogNode} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  {Uin, OrderID, BillNo, Account, GoodID, GoodPrice, AmountFee, Reason} = PayRd,
  Format =
    [
      "LogDB|", TableName,"|\"",
      dd_util:to_list(Now), "\",\"",
      dd_util:to_list(Key), "\",",
      dd_util:to_list(Uin), ",\"",
      "KY", "\",\"",
      dd_util:to_list(OrderID), "\",\"",
      dd_util:to_list(BillNo), "\",\"",
      dd_util:to_list(Account), "\",\"",
      dd_util:to_list(GoodID), "\",\"",
      dd_util:to_list(GoodPrice), "\",\"",
      dd_util:to_list(AmountFee), "\",\"",
      dd_util:to_list(Reason), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(LogNode, log, write_log, [LogBin]),
  ?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({write_360_pay, {TableName, PayRd}},  #state{log_node = LogNode} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  {Uin, OrderID, BillNo, Account, GoodID, GoodPrice, Amount360, Reason} = PayRd,
  Format =
    [
      "LogDB|", TableName,"|\"",
      dd_util:to_list(Now), "\",\"",
      dd_util:to_list(Key), "\",",
      dd_util:to_list(Uin), ",\"",
      "360", "\",\"",
      dd_util:to_list(OrderID), "\",\"",
      dd_util:to_list(BillNo), "\",\"",
      dd_util:to_list(Account), "\",\"",
      dd_util:to_list(GoodID), "\",\"",
      dd_util:to_list(GoodPrice), "\",\"",
      dd_util:to_list(Amount360), "\",\"",
      dd_util:to_list(Reason), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(LogNode, log, write_log, [LogBin]),
  ?FILE_LOG_DEBUG("log=~p", [LogBin]),
  {noreply, State};
handle_cast({write_pp_pay, {TableName, PayRd}},  #state{log_node = LogNode} = State) ->
  Now = dd_util:time_format(),
  Key = dd_util:time_key(),
  {Uin, OrderID, BillNo, Account, GoodID, GoodPrice, PPAmount, Reason} = PayRd,
  Format =
    [
      "LogDB|", TableName,"|\"",
      dd_util:to_list(Now), "\",\"",
      dd_util:to_list(Key), "\",",
      dd_util:to_list(Uin), ",\"",
      "PP", "\",\"",
      dd_util:to_list(OrderID), "\",\"",
      dd_util:to_list(BillNo), "\",\"",
      dd_util:to_list(Account), "\",\"",
      dd_util:to_list(GoodID), "\",\"",
      dd_util:to_list(GoodPrice), "\",\"",
      dd_util:to_list(PPAmount), "\",\"",
      dd_util:to_list(Reason), "\",\"\""
    ],
  LogBin = lists:flatten(Format),
  rpc:cast(LogNode, log, write_log, [LogBin]),
  ?FILE_LOG_DEBUG("log=~p", [LogBin]),
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
