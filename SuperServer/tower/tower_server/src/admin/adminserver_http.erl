%% @author {{author}}
%% @copyright {{year}} {{author}}

%% @doc Web server for {{appid}}.

-module(adminserver_http).

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
  try
      "/" ++ Modname  = Req:get(path),
    mod_check(Modname),
    AtomModName = list_to_atom("mod_"++Modname),
	io:format("now in admin_http is ---~p~n",[Modname]),
	io:format("now in admin_http data is ---~p~n",[Req:parse_post()]),
    Result = AtomModName:req_handle(Req),           %不同的模块通过AromModName变量调用Req_handle
	io:format("now in admin_http respond is ---~p~n",[Result]),
    Req:respond({200, [], dd_util:encode_json_utf8(Result)})
  catch
    throw:{custom, Reason} ->
      ?FILE_LOG_WARNING("http request error, reason = ~p", [Reason]),
      RetJsonStruct1 = {struct, [{<<"result">>, -1}, {<<"error">>, "HintSystemError"}]},
      Req:respond({200, [], dd_util:encode_json_utf8(RetJsonStruct1)});
    Type:What ->
      ?FILE_LOG_ERROR("exception type=~p, what=~p,stack=~p", [Type, What, erlang:get_stacktrace()]),
      RetJsonStruct4 = {struct, [{<<"result">>, -1}, {<<"error">>, <<"unknown error">>}]},
      Req:respond({200, [], dd_util:encode_json_utf8(RetJsonStruct4)})
  end.

mod_check("backpack_oper") -> ok;
mod_check("admin_login")->ok;
mod_check("admin_add_goal")->ok;
mod_check("admin_add_tool")->ok;
mod_check("admin_action")->ok;
mod_check("query_all_mail_template")->ok;
mod_check("add_mail_template")->ok;
mod_check("query_mail_template")->ok;
mod_check("add_attach_mail")->ok;
mod_check("query_all_bulletin_mail")->ok;
mod_check("add_bulletin_mail")->ok;
mod_check("delete_bulletin")->ok;
mod_check("delete_template")->ok;
mod_check("search_player")->ok;
mod_check("fresh_player")->ok;
mod_check("distribution_gift")->ok;
mod_check("stop_server_time")->ok;
mod_check("add_shop_item")->ok;
mod_check("delete_shop_item")->ok;
mod_check("query_shop_item")->ok;
mod_check("change_game_model_function")->ok;
mod_check("admin_delete_account") -> ok;
mod_check("query_notice") -> ok;
mod_check("query_all_notice") -> ok;
mod_check("notice_manager") -> ok;

mod_check(Others)->
  Reason = "error request path:" ++ Others,
  Value = {struct,[{<<"result">>, -1},{<<"error">>, dd_util:to_binary(Reason)}]},
  throw({custom, Value}).

%% Internal API

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

