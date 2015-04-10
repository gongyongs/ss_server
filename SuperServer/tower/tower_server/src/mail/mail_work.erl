%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 九月 2014 下午7:09
%%%-------------------------------------------------------------------
-module(mail_work).
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
  send_feedback_mail_cast/2,
  filter_mail_cast/2
]).

-define(SERVER, ?MODULE).

-include("../../deps/file_log/include/file_log.hrl").

-export([execute/3,filter_mail/2,send_feedback_mail/2]).

-record(state, {
}).

%%%===================================================================
%%% API
%%%===================================================================
execute(ProcName, FuncName, FuncParam) ->
  gen_server:call(ProcName, {event, {FuncName, FuncParam}}).

filter_mail(Uin, Type)->
  ProcName = mail_sup:hash_uin_to_proc(Uin),
  gen_server:cast(ProcName, {filter_mail, {Uin, Type}}).

send_feedback_mail_cast(FeedBackData,Uin)->
  ?FILE_LOG_DEBUG("send_feedback_mail_cast",[]),
  ProcName = mail_sup:hash_uin_to_proc(Uin),
  gen_server:cast(ProcName, {send_feedback_mail, {FeedBackData, Uin}}).

%set_online_user_count(OnlineNumber) when is_integer(OnlineNumber)->
%  gen_server:cast(?MODULE, {set_online_user_count, OnlineNumber}).

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
  {success, DbNode} = dd_ms:read_config(database_node),
  dd_config:write_cfg(database_node, DbNode),
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
handle_call({event, {FuncName, FuncParam}}, _From, State) ->
  RetValue = work_proc({execute, {FuncName, FuncParam}}),
  {reply, RetValue, State};
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
handle_cast({filter_mail, {Uin, MailType}}, State) ->
  spawn(?MODULE,filter_mail_cast,[Uin,MailType]),  % spawn(?MODULE, work_proc, Param),        % 1  好友邮件   2  系统邮件
  {noreply, State};
handle_cast({send_feedback_mail, {FeedBackData, Uin}}, State) ->
  spawn(?MODULE,send_feedback_mail,[FeedBackData, Uin]),  % spawn(?MODULE, work_proc, Param),        % 1  好友邮件   2  系统邮件
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
work_proc(Param) ->
  case Param of
    {execute, {FuncName, FuncParam}} ->
      try
        ?FILE_LOG_DEBUG("GO INTO mail_work=>work_proc ", []),
        mail_work_impl:execute(FuncName, FuncParam)
      catch
        throw:{custom, Reason} ->
          ?FILE_LOG_WARNING("custom reason :~p", [Reason]),
          {fail, Reason};
        What:Type ->
          ?FILE_LOG_ERROR(
            "what=~p, type=~p, stack=~p",
            [What, Type, erlang:get_stacktrace()]
          ),
          {fail, "HintSystemDataError"}
      end;
    Other ->
      ?FILE_LOG_DEBUG("mail_work:work_proc receive = [~p]", [Other]),
      success
  end.


filter_mail_cast(Uin,MailType) ->
  try
    mail_db:filter_mail_from_db(Uin,MailType)
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_WARNING("custom reason :~p", [Reason]),
      {fail, Reason};
    What:Type ->
      ?FILE_LOG_ERROR(
        "what=~p, type=~p, stack=~p",
        [What, Type, erlang:get_stacktrace()]
      ),
      {fail, "HintSystemDataError"}
  end.

send_feedback_mail(FeedBackData, Uin)->
  ?FILE_LOG_DEBUG("send_feedback_mail function",[]),
  try
    mail_work_impl:send_feedback_mail(FeedBackData, Uin)
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_WARNING("send_feedback_mail error, reason :~p", [Reason]);
    What:Type ->
      ?FILE_LOG_ERROR(
        "send_feedback_mail error, what=~p, type=~p, stack=~p",
        [What, Type, erlang:get_stacktrace()]
      )
  end.
