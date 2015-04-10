%% @author {{author}}
%% @copyright {{year}} {{author}}

%% @doc Web server for {{appid}}.

-module(adminserver_web).
-author("jiajia").

-export([start/1, stop/0, loop/2]).
-include("../../deps/file_log/include/file_log.hrl").
%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _) ->
    "/" ++ Path = Req:get(path),
    try
      AtomModName = list_to_atom("mod_admin_"++Path),
	io:format("now in admin_web is ---~p~n",[AtomModName]),
	io:format("now in admin_web data is ---~p~n",[Req:parse_post()]),
      Result = AtomModName:handleReq(Req),
      %Req:respond({200, [], mochijson2:encode(Result)})
	io:format("now in admin_web respond is ---~p~n",[Result]),
      Req:respond({200, [], Result})
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            ?FILE_LOG_ERROR("exception type=~p, what=~p,stack=~p", [Type, What, erlang:get_stacktrace()]),
            %% NOTE: mustache templates need \\ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\\n"})
    end.

%% Internal API

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

