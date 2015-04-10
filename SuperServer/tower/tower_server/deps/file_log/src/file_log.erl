-module(file_log).
-author('erlangonrails@gmail.com').
-include("file_log_internal.hrl").
-export([start/0, stop/0]).
-export([set/1]).
-export([mod_1/0, mod_nil_1/0,
         mod_2/0, mod_nil_2/0,
         mod_3/0, mod_nil_3/0,
         mod_4/0, mod_nil_4/0]).

start() ->
    application:start(file_log).

stop() ->
    application:stop(file_log).


-spec set(Level :: file_log_level()) -> ok | {error, term()}.
set(error) ->    build_log_mod(1);
set(warning) ->  build_log_mod(2);
set(info) ->     build_log_mod(3);
set(debug) ->    build_log_mod(4).

%% Internal APIs:
build_log_mod(N) ->
    M = file_dynamic:new(?FILE_LOGMOD),
    {ok, M1} = file_dynamic:add_fun(M, mod_open(), true),
    {ok, M2} = file_dynamic:add_fun(M1, mod_close(), true),
    M3 = 
      lists:foldl(
        fun(I, Acc) ->
            TmpF = 
              case I =< N of
                  true ->
                      list_to_atom("mod_" ++ integer_to_list(I));
                  false ->
                      list_to_atom("mod_nil_" ++ integer_to_list(I))
              end,
            {ok, AccOut} = 
              file_dynamic:add_fun(Acc, apply(?MODULE, TmpF, []), true),
            AccOut
        end, M2, lists:seq(1, 4)),
    file_dynamic:compile(M3).

mod_open() ->
    "open(Filename) ->"
    "    {ok, Fd} = file:open(Filename, [append]),"
    "    {ok, Fd}.".

mod_close() ->
    "close(Fd) ->"
    "    file:close(Fd).". 

mod_1() ->
    "error(Fd, Pid, Module, Line, Fmt, Args) ->"
    "    Msg = io_lib:format(Fmt, Args),"
    "    {{Y,Mo,D},{H,Mi,S}} = erlang:localtime(),"
    "    io:format(Fd,\"[error] [~p:~p:~p] [~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w] ~s~n\", "
    "      [Pid,Module,Line,Y,Mo,D,H,Mi,S,Msg]).".
mod_nil_1() ->
    "error(_Fd, _Pid, _Module, _Line, _Fmt, _Args) ->"
    "    ok.".

mod_2() ->
    "warning(Fd, Pid, Module, Line, Fmt, Args) ->"
    "    Msg = io_lib:format(Fmt, Args),"
    "    {{Y,Mo,D},{H,Mi,S}} = erlang:localtime(),"
    "    io:format(Fd,\"[warning] [~p:~p:~p] [~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w] ~s~n\", "
    "      [Pid,Module,Line,Y,Mo,D,H,Mi,S,Msg]).".
mod_nil_2() ->
    "warning(_Fd, _Pid, _Module, _Line, _Fmt, _Args) ->"
    "    ok.".

mod_3() ->
    "info(Fd, Pid, Module, Line, Fmt, Args) ->"
    "    Msg = io_lib:format(Fmt, Args),"
    "    {{Y,Mo,D},{H,Mi,S}} = erlang:localtime(),"
    "    io:format(Fd,\"[info] [~p:~p:~p] [~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w] ~s~n\", "
    "      [Pid,Module,Line,Y,Mo,D,H,Mi,S,Msg]).".
mod_nil_3() ->
    "info(_Fd, _Pid, _Module, _Line,_Fmt, _Args) ->"
    "    ok.".

mod_4() ->
    "debug(Fd, Pid, Module, Line, Fmt, Args) ->"
    "    Msg = io_lib:format(Fmt, Args),"
    "    {{Y,Mo,D},{H,Mi,S}} = erlang:localtime(),"
    "    io:format(Fd,\"[debug] [~p:~p:~p] [~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w] ~s~n\", "
    "      [Pid,Module,Line,Y,Mo,D,H,Mi,S,Msg]).".
mod_nil_4() ->
    "debug(_Fd, _Pid, _Module, _Line, _Fmt, _Args) ->"
    "    ok.".


