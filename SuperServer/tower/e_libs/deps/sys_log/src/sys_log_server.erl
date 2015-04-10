-module(sys_log_server).
-author('longtugame').
-include("sys_log.hrl").
-include("file_log.hrl").
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
  start_link/0,
  write_admin_log/1,
  write_sys_log/1,
  write_player_log/1,
  write_error_log/4,
  write/4
]).
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
write_sys_log(Param)->
  gen_server:cast(?MODULE,{write_sys_log,[Param]}).

write_admin_log(Param)->
  gen_server:cast(?MODULE,{write_admin_log,[Param]}).

write_player_log(Param)->
  gen_server:cast(?MODULE,{write_player_log,[Param]}).

write(MODULE,LINE,Format,Param)->
  gen_server:cast(?MODULE,{write_log,[MODULE,LINE,Format,Param]}).

write_error_log(MODULE,LINE,Format,Param)->
  gen_server:cast(?MODULE,{write_error_log,[MODULE,LINE,Format,Param]}).

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
handle_cast({write_sys_log,[Param]},State)->
  write_to_file(system,Param),
  {noreply, State};
handle_cast({write_admin_log,[Param]},State)->
  write_to_file(admin,Param),
  {noreply, State};
handle_cast({write_player_log,[Param]},State)->
  write_to_file(player,Param),
  {noreply, State};
handle_cast({write_log,[MODULE,LINE,Format,Param]},State)->
  write_log_file(MODULE,LINE,Format,Param),
  {noreply, State};
handle_cast({write_error_log,[MODULE,LINE,Format,Param]},State)->
  write_error_log_file(MODULE,LINE,Format,Param),
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
handle_info({timeout, _TimerRef, 'update_online_user_count'}, #state{} = State) ->
  {noreply, State};
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

%%
write_to_file(LogType,Param) when is_list(Param) ->
  {{Y, M, D},_HSM} = calendar:local_time(),
  Dir_name = dd_util:to_list(Y) ++ "_" ++ dd_util:to_list(M) ++ "_" ++ dd_util:to_list(D),

  RootPath = os:getenv("PUBLIC_LOG_PATH"),
  DirPath = dd_util:to_list(RootPath) ++ "/var/log/log_file/" ++ Dir_name ++"/",

  File_name =
    case LogType of
      admin -> "admin_log.csv";
      system -> "system_log.csv";
      player -> "player_log.csv"
    end,
  File = DirPath ++ File_name,
  ok = filelib:ensure_dir(DirPath),
  {ok, Fd} = file:open(File, [write,read,append]),
  Type = encode_param_type(Param),
  Time = dd_util:time_format(),
  NParam = [Time|Param],
  NType = "~p|" ++ Type,
  io:format(Fd, NType, NParam),
  file:close(Fd).

%%类似file_log_info("",[])功能
write_log_file(MODULE,LINE,Format,Param)when is_list(Format) ->
  {{Y, M, D},_HSM} = calendar:local_time(),
  Dir_name = dd_util:to_list(Y) ++ "_" ++ dd_util:to_list(M) ++ "_" ++ dd_util:to_list(D),
  RootPath = os:getenv("PUBLIC_LOG_PATH"),
  DirPath = dd_util:to_list(RootPath) ++ "/var/log/log_file/" ++ Dir_name ++"/",
  File_name = "common_log.csv",    %%公共日志文件
  File = DirPath ++ File_name,
  ok = filelib:ensure_dir(DirPath),
  {ok, Fd} = file:open(File, [write,read,append]),
  NFormat = "[COMMON] [~p:~p][~p]" ++ Format ++ "~n",       %%[Module:Line] [Time]
  Time = dd_util:time_format(),
  NParam = [MODULE,LINE,Time|Param],
  io:format(Fd, NFormat, NParam),
  file:close(Fd).

%%类似file_log_error("",[])功能
write_error_log_file(MODULE,LINE,Format,Param)when is_list(Format) ->
  {{Y, M, D},_HSM} = calendar:local_time(),
  Dir_name = dd_util:to_list(Y) ++ "_" ++ dd_util:to_list(M) ++ "_" ++ dd_util:to_list(D),
  RootPath = os:getenv("PUBLIC_LOG_PATH"),
  DirPath = dd_util:to_list(RootPath) ++ "/var/log/log_file/" ++ Dir_name ++"/",
  File_name = "error_log.csv",    %%公共日志文件
  File = DirPath ++ File_name,
  ok = filelib:ensure_dir(DirPath),
  {ok, Fd} = file:open(File, [write,read,append]),
  NFormat = "[ERROR] [~p:~p][~p]" ++ Format ++ "~n",
  Time = dd_util:time_format(),
  NParam = [MODULE,LINE,Time|Param],
  io:format(Fd, NFormat, NParam),
  file:close(Fd).

encode_param_type(Param) ->          %%根据参数给出io输出类型
  Param_length = length(Param),
  Type=
    lists:foldr(
      fun(_Para,SType) ->
        SType ++ "|~p" end,"~p",lists:seq(1,Param_length-1)),
  Type ++ "~n".







