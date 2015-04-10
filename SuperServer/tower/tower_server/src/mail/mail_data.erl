%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 十二月 2014 下午2:54
%%%-------------------------------------------------------------------
-module(mail_data).
-author("zqlt").

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
%%早上两点半清除过时邮件
-define(FILTER_MAIL_TIME, {2, 30, 0}).
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

-export([
  delete_mail_overtime_func/2
]).

-record(state, {
  last_filter_mail_ts::integer()   %%上次过滤邮件的时间
}).

%%%===================================================================
%%% API
%%%===================================================================

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
  ?FILE_LOG_DEBUG("START TIMER",[]),
  erlang:start_timer(2*60*60000, self(), 'delete_mail_overtime'),  %%删除过时邮件     time单位ms  60000一分钟
  {ok, #state{last_filter_mail_ts = 0}}.

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
handle_info({timeout, _TimerRef, 'delete_mail_overtime'},  #state{last_filter_mail_ts = LastSettlementTs} = State) ->
  Pid = self(),
  spawn(?MODULE, delete_mail_overtime_func, [Pid,LastSettlementTs]),
  erlang:start_timer(2*60*60000, self(), 'delete_mail_overtime'),  %%结算
  {noreply, State};
handle_info(delete_mail_overtime_finished, State) ->
  ?FILE_LOG_DEBUG("FILTER MAIL FINISH", []),
  {noreply, State#state{last_filter_mail_ts = dd_util:timestamp()}};
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

delete_mail_overtime_func(ParentPid, LastSettlementTs) ->
  case should_be_settlement(LastSettlementTs) of
    false -> ok;
    true ->
      ?FILE_LOG_INFO("START DELETE MAIL OVERTIME",[]),
      try
        mail_db:filter_mail_overtime(),
        ParentPid ! delete_mail_overtime_finished
      catch
        throw:{custom, Reason} ->
          ?FILE_LOG_WARNING("custom reason :~p", [Reason]);
        What:Type ->
          ?FILE_LOG_ERROR(
            "what=~p, type=~p, stack=~p",
            [What, Type, erlang:get_stacktrace()]
          )
      end
  end.

should_be_settlement(LastSettlementTs) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
  if
    true ->
      %%是否到结算时间
      case dd_util:time_compare_by_datetime({{Year, Month, Day}, {Hour, Minute, Second}}, {{Year, Month, Day}, ?FILTER_MAIL_TIME}) of
        1 -> %%达到结算时间,判断上一次结算时间
          LastSettlementDateTime = dd_util:to_local_time(dd_util:timestamp_to_datetime(LastSettlementTs)),
          case dd_util:time_compare_by_datetime(LastSettlementDateTime, {{Year, Month, Day}, ?FILTER_MAIL_TIME}) of
            -1 -> %%未结算
              true;
            _ -> false
          end;
        _ -> false
      end
  end.