-module(file_log_server).
-author('erlangonrails@gmail.com').
-include("file_log_internal.hrl").
-behaviour(gen_server).

-export([start_link/0, 
         start_link/1,
         send/6,
         set/1,
         get/0,
         get_rotate_interval/0,
         rotate/0]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    start_link(get_log_config()).

-record(state, {fd :: file:io_device(),
                loglevel :: file_log_level(),
                rawlogpath :: string(),                             %% 配置文件中的日志路径
                curlogpath :: string(),                             %% 当前使用的日志路径
                timerref :: reference(),                            %% rotate_log的定时器
                rotate_interval :: infinity | non_neg_integer()}).  %% seconds

-spec start_link({LogPath :: string(),
                  LogLevel :: file_log_level(),
                  RotateInterval :: infinity | non_neg_integer()}) -> {ok, pid()}.
start_link({LogPath, LogLevel, RotateInterval}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LogPath, LogLevel, RotateInterval], []).

-spec send(Type :: file_log_level(),
           Pid :: pid(),
           Module :: module(),
           Line :: non_neg_integer(),
           Fmt :: string(),
           Args :: list()) -> ok.
send(Type, Pid, Module, Line, Fmt, Args) ->
    gen_server:cast(?MODULE, {send_log, Type, Pid, Module, Line, Fmt, Args}).


-spec get() -> file_log_level().
get() ->
    gen_server:call(?MODULE, get, infinity).

-spec get_rotate_interval() -> infinity | non_neg_integer().
get_rotate_interval() ->
    gen_server:call(?MODULE, get_rotate_interval, infinity).

-spec set(LogLevel :: file_log_level()) -> ok.
set(LogLevel) ->
    gen_server:call(?MODULE, {set, LogLevel}, infinity).

-spec rotate() -> ok.
rotate() ->
    gen_server:call(?MODULE, rotate_log, infinity).

init([LogPath, LogLevel, RotateInterval]) ->
    ok = file_log:set(LogLevel),
    ok = filelib:ensure_dir(LogPath),
    CurLogPath = lists:flatten([filename:rootname(LogPath), get_localtime_str()]),
    {ok, Fd} = ?FILE_LOGMOD:open(CurLogPath),
    {ok, TRef} = timer:apply_interval(RotateInterval * 1000, ?MODULE, rotate, []),
    {ok, #state{fd = Fd,
                loglevel = LogLevel,
                rawlogpath = LogPath,
                curlogpath = CurLogPath,
                timerref = TRef,
                rotate_interval = RotateInterval}}.

handle_call({set, LogLevel}, _From, State) ->
    %% 重新生成日志模块, 但我们仍然可以使用之前的文件描述符
    file_log:set(LogLevel),
    {reply, ok, State#state{loglevel = LogLevel}};
handle_call(get, _From, #state{loglevel = Level} = State) ->
    {reply, Level, State};
handle_call(get_rotate_interval, _From, #state{rotate_interval = Interval} = State) ->
    {reply, Interval, State};
handle_call(rotate_log, _From, #state{fd = Fd,
                                      rawlogpath = RawLogPath,
                                      curlogpath = CurLogPath} = State) ->
    ?FILE_LOGMOD:close(Fd),
    RotationName = filename:rootname(CurLogPath),
    file:rename(CurLogPath, [RotationName, "__", get_localtime_str()]),
    NewCurLogPath = lists:flatten([filename:rootname(RawLogPath), get_localtime_str()]),
    {ok, NewFd} = ?FILE_LOGMOD:open(NewCurLogPath),
    {reply, ok, State#state{fd = NewFd, curlogpath = NewCurLogPath}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({send_log, Type, Pid, Module, Line, Fmt, Args}, State) ->
    ?FILE_LOGMOD:Type(State#state.fd, Pid, Module, Line, Fmt, Args),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{fd = Fd, timerref = TRef}) ->
    catch ?FILE_LOGMOD:close(Fd),
    catch timer:cancel(TRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal APIs:
get_localtime_str() ->
    {{Y,M,D},{Hour, Min, Sec}} = calendar:local_time(),
    io_lib:format("_~p_~p_~p_~p_~p_~p.log", [Y, M, D, Hour, Min, Sec]).

-spec get_log_config() ->
    {LogPath :: string(),
     LogLevel :: file_log_level(),
     RotateInterval :: infinity | non_neg_integer()}.
get_log_config() ->
    Path = os:getenv("LOG_PATH"),
    Level = os:getenv("LOG_LEVEL"),
    RotateInterval = os:getenv("LOG_ROTATE_INTERVAL"),
    {Path, list_to_atom(Level), list_to_integer(RotateInterval)}.
