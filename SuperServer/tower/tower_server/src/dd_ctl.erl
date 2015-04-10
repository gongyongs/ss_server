%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 七月 2014 下午8:20
%%%-------------------------------------------------------------------
-module(dd_ctl).
-author("zqlt").

%% API
-export([start/0, process/1]).
-export([get_master_node/0,
  get_mnesia_path/0,
  get_ebin_path/0,
  get_pid_path/0,
  write_pid_file/0,
  delete_pid_file/0]).

-export([
]).

-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).
-define(STATUS_BADRPC,  3).

-spec get_master_node() -> undefined | string().
get_master_node() ->
  case os:getenv("MASTER_NODE") of
    false -> undefined;
    Path -> Path
  end.

-spec get_mnesia_path() -> undefined | string().
get_mnesia_path() ->
  case os:getenv("MNESIA_PATH") of
    false -> undefined;
    Path -> Path
  end.

-spec get_ebin_path() -> undefined | string().
get_ebin_path() ->
  case os:getenv("EBIN_PATH") of
    false -> undefined;
    Path -> Path
  end.

-spec get_pid_path() -> undefined | string().
get_pid_path() ->
  case os:getenv("PID_PATH") of
    false -> undefined;
    Path -> Path
  end.

-spec write_pid_file() -> ok.
write_pid_file() ->
  case ?MODULE:get_pid_path() of
    undefined ->
      ok;
    PidFilename ->
      write_pid_file(os:getpid(), PidFilename)
  end.

-spec write_pid_file(Pid :: string(), PidFilename :: string()) -> ok.
write_pid_file(Pid, PidFilename) ->
  case file:open(PidFilename, [write]) of
    {ok, Fd} ->
      io:format(Fd, "~s~n", [Pid]),
      file:close(Fd);
    {error, Reason} ->
      throw({cannot_write_pid_file, PidFilename, Reason})
  end.

-spec delete_pid_file() -> ok.
delete_pid_file() ->
  case ?MODULE:get_pid_path() of
    undefined -> ok;
    PidFilename -> file:delete(PidFilename)
  end.

start() ->
  case init:get_plain_arguments() of
    [SNode | Args] ->
      Node = list_to_atom(SNode),
      Status =
        case rpc:call(Node, ?MODULE, process, [Args]) of
          {badrpc, Reason} ->
            io:format("RPC failed on the node ~p: ~p~n",
              [Node, Reason]),
            ?STATUS_BADRPC;
          S -> S
        end,
      halt(Status);
    _ ->
      print_usage(),
      halt(?STATUS_USAGE)
  end.

process(["stop"]) ->
  init:stop(),
  ?STATUS_SUCCESS;
process(["restart"]) ->
  init:restart(),
  ?STATUS_SUCCESS;
process(["status", AppName]) ->
  AtomName = list_to_atom(AppName),
  {InternalStatus, ProvidedStatus} = init:get_status(),
  io:format("Node ~p is ~p. Status: ~p~n",
    [node(), InternalStatus, ProvidedStatus]),
  case lists:keysearch(AtomName, 1, application:which_applications()) of
    false ->
      io:format("~p is not running~n", [AtomName]),
      ?STATUS_ERROR;
    {value, {_, _, Version}} ->
      io:format("~p ~s is running in that node~n", [AtomName, Version]),
      ?STATUS_SUCCESS
  end;
process(["set-loglevel", Level]) ->
  AtomLevel = list_to_atom(string:to_lower(Level)),
  case lists:member(AtomLevel, ['error','warning','info','debug']) of
    true ->
      file_log_server:set(AtomLevel),
      io:format("set log level(~p) success.~n", [AtomLevel]),
      ?STATUS_SUCCESS;
    false ->
      io:format(
        "set log level failed.~n"
        "$usage$:~n"
        "ctl set-loglevel (error|warning|info|debug)~n", []),
      ?STATUS_ERROR
  end;
process(["get-loglevel"]) ->
  Level = file_log_server:get(),
  io:format("current log level:~p~n", [Level]),
  ?STATUS_SUCCESS;
process(["get-rotate"]) ->
  Interval = file_log_server:get_rotate_interval(),
  {H, M, S} = seconds_to_time(Interval),
  io:format(
    "log rotate interval:~p hours, ~p minutes, ~p seconds~n",
    [H, M, S]),
  ?STATUS_SUCCESS;
process(["rotate-log"]) ->
  file_log_server:rotate(),
  io:format("rotate log success.~n", []),
  ?STATUS_SUCCESS;
process(_) ->
  print_usage(),
  ?STATUS_ERROR.


print_usage() ->
  io:format("usage: ctl~n"
  "~n"
  "The most commonly used master_ctl commands are:~n"
  "  start         Start halld service~n"
  "  stop          Stop halld service~n"
  "  restart       Restart halld service~n"
  "  status        Display the status of halld service~n"
  "  debug         Enter the debug console~n"
  "  set-loglevel  Set the log level (error|warning|info|debug)~n"
  "  get-loglevel  Get the log level~n"
  "  get-rotate    Get the log rotate interval~n"
  "  rotate-log    Rotate log file~n"
  "  help          Print the usage~n"
  "~n"
  "See 'ctl help <command>' for more information on a specific command.~n").


%% 将秒转换为{Hour, Minute, Second}格式的时间, 方便阅读.
%%
%% 备注:
%% calendar:seconds_to_time/1参数必须小于86400, 该API
%% 没有这个限制.
-spec seconds_to_time(Seconds :: integer()) ->
  {H :: integer(), M :: integer(), S :: integer()}.
seconds_to_time(Seconds) when Seconds < 86400 ->
  calendar:seconds_to_time(Seconds);
seconds_to_time(Seconds) ->
  D = Seconds div 86400,
  {H, M, S} = calendar:seconds_to_time(Seconds rem 86400),
  {H + D * 24, M, S}.
