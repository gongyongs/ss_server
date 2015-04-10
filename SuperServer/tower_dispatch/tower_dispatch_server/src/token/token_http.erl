%% @author Mochi Media <dev@mochimedia.com>

%% @copyright 2010 Mochi Media <dev@mochimedia.com>



%% @doc Web server for payment.



-module(token_http).

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
      "/" ++ ModName = Req:get(path),
    %%去除后缀
    mod_check(ModName),
    AtomModName = list_to_atom("mod_" ++ ModName),
	io:format("now in token is ---~p~n",[ModName]),
    JsonStruct = AtomModName:req_handle(Req),
	io:format("now in token JsonStruct is ---~p~n",[JsonStruct]),
    Req:respond({200, [], mochijson2:encode(JsonStruct)})
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_ERROR("token http error, reason = ~p", [Reason]),
      RetJsonStruct3 = {struct, [{<<"result">>, -1}, {<<"error">>, <<"HintSystemError">>}]},
      Req:respond({200, [], mochijson2:encode(RetJsonStruct3)});
    Type:What ->
      ?FILE_LOG_ERROR("exception type=~p, what=~p, stack=~p", [Type, What, erlang:get_stacktrace()]),
      RetJsonStruct4 = {struct, [{<<"result">>, -1}, {<<"error">>, <<"HintSystemError">>}]},
      Req:respond({200, [], mochijson2:encode(RetJsonStruct4)})
  end.


mod_check("token_verify") -> ok;
mod_check("game_server_register") -> ok;
mod_check("check_register_user_data") -> ok;
mod_check("request_friend_data") -> ok;
mod_check("version_notify") -> ok;
mod_check(Other) ->
  throw({custom, "error request:" ++ Other}).

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
