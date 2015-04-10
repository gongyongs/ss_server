%% @author Mochi Media <dev@mochimedia.com>

%% @copyright 2010 Mochi Media <dev@mochimedia.com>



%% @doc Web server for payment.



-module(http_proc_web).

-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

-include("../../deps/file_log/include/file_log.hrl").







start(Options) ->

  {DocRoot, Options1} = get_option(docroot, Options),

  Loop =

    fun(Req) ->

      ?MODULE:loop(Req, DocRoot)

    end,

  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).



stop() ->

  mochiweb_http:stop(?MODULE).



loop(Req, _DocRoot) ->
  try
      "/" ++ Path = Req:get(path),
    ?FILE_LOG_DEBUG("path = ~p", [Path]),
	io:format("now in http_proc_web is ---~p~n",[Path]),
    case http_proc_util:get_pay_cb_notify_by_path(Path) of
      fail ->  throw({custom, "ErrorPath"});
      {success, {Mod, _, _}} ->
        RetData = Mod:call_back_handle(Req),
        Req:respond({200, [], RetData})
    end
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_WARNING("http request error reason=~p", [Reason]),
      RetJsonStruct1 = {struct, [{<<"result">>, -1}, {<<"error">>, <<"path error">>}]},
      Req:respond({500, [], dd_util:encode_json_utf8(RetJsonStruct1)});
    Type:What ->
      ?FILE_LOG_ERROR("exception type=~p, what=~p, stack=~p", [Type, What, erlang:get_stacktrace()]),
      RetJsonStruct4 = {struct, [{<<"result">>, -1}, {<<"error">>, <<"unknown error">>}]},
      Req:respond({500, [], dd_util:encode_json_utf8(RetJsonStruct4)})
  end.






%% Internal API




get_option(Option, Options) ->

  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").



you_should_write_a_test() ->

  ?assertEqual(

    "No, but I will!",

    "Have you written any tests?"),

  ok.



-endif.


