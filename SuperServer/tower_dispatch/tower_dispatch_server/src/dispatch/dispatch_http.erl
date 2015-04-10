%% @author Mochi Media <dev@mochimedia.com>

%% @copyright 2010 Mochi Media <dev@mochimedia.com>



%% @doc Web server for payment.



-module(dispatch_http).

-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

-include("../../deps/file_log/include/file_log.hrl").
-include("concurrency_test.hrl").


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
    "/" ++ ModName = Req:get(path),
	PostData = Req:parse_post(),
	io:format("now in dispatch is ---~p~n",[ModName]),
	io:format("now in dispatch data is ---~p~n",[PostData]),
  RequestRd = #request_rd{request_id = dd_util:to_list(ModName), request_cost = 0, response_result = -1, request_over_tms = 0, request_start_tms = dd_util:milliseconds(), request_time = dd_util:time_format()},
  try
    ?FILE_LOG_DEBUG("name = ~p", [ModName]),
    %%去除后缀
    mod_check(ModName),
    AtomModName = list_to_atom("mod_" ++ ModName),
    JsonStruct = AtomModName:req_handle(Req),

    %%输出统计结果
    {struct, [{<<"result">>, ResultVal} | _]} = JsonStruct,
    concurrency_test:cast_result(ResultVal, RequestRd),
	io:format("now in dispatch  respond is ---~p~n",[JsonStruct]),
    Req:respond({200, [], dd_util:encode_json_utf8(JsonStruct)})
  catch
    {custom, Reason} ->
      ?FILE_LOG_ERROR("dispatch error, reason = ~p", [Reason]),
      RetJson = {struct, [{<<"result">>, -1}, {<<"error">>, Reason}]},
      %%concurrency_test:cast_result(-1, RequestRd),
      Req:respond({200, [], dd_util:encode_json_utf8(RetJson)});
    Type:What ->
      ?FILE_LOG_ERROR("exception type=~p, what=~p, stack=~p", [Type, What, erlang:get_stacktrace()]),
      RetJsonStruct4 = {struct, [{<<"result">>, -1}, {<<"error">>, <<"HintSystemError">>}]},
      %%concurrency_test:cast_result(-1, RequestRd),
      Req:respond({200, [], dd_util:encode_json_utf8(RetJsonStruct4)})
  end.


mod_check("login") -> ok;
mod_check("register") -> ok;
mod_check("check_device") -> ok;
mod_check("bond_device") -> ok;
mod_check("pp_login") -> ok;
mod_check("pp_check_device") -> ok;
mod_check(_Other) ->
  throw({custom, "HintInvalidRequest"}).

%% Internal API




get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.



%%

%% Tests

%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").



you_should_write_a_test() ->

  ?assertEqual(

    "No, but I will!",

    "Have you written any tests?"),

  ok.



-endif.

