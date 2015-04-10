-module(file_log_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    LogSup = 
      {file_log_sup,
        {file_log_server, start_link, []},
        permanent,
        brutal_kill,
        supervisor,
        [file_log_server]},
    {ok, {{one_for_one, 100, 5},
         [LogSup]}}.
