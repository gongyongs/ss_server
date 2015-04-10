%% @author Mochi Media <dev@mochimedia.com>

%% @copyright 2010 Mochi Media <dev@mochimedia.com>



%% @doc Web server for payment.



-module(gateway_http).

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
  RequestRd = #request_rd{request_id = dd_util:to_list(ModName), request_cost = 0, response_result = -1, request_over_tms = 0, request_start_tms = dd_util:milliseconds(), request_time = dd_util:time_format()},
  try

    mod_check(ModName),
    AtomModName = list_to_atom("mod_" ++ ModName),
    ?FILE_LOG_INFO("gateway_http request: node = ~p, request = ~p", [node(), AtomModName]),
    Json = AtomModName:req_handle(Req),

    %%输出统计结果
    {struct, [{<<"result">>, ResultVal} | _]} = Json,
    concurrency_test:cast_result(ResultVal, RequestRd),

    ?FILE_LOG_DEBUG("request_name:~p, request_param:~p, response_data:~p", [AtomModName, Req, Json]),
    Req:respond({200, [], dd_util:encode_json_utf8(Json)})
  catch
    {custom, Reason} ->
      ?FILE_LOG_ERROR("gateway http request error, reason = ~p, stack = ~p", [Reason ,erlang:get_stacktrace()]),
      RetJson = {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]},
      concurrency_test:cast_result(-1, RequestRd),
      Req:respond({200, [], dd_util:encode_json_utf8(RetJson)});
    Type:What ->
        ?FILE_LOG_ERROR("gateway http request exception type=~p, what=~p, stack=~p", [Type, What, erlang:get_stacktrace()]),
        RetJsonR = {struct, [{<<"result">>, -1}, {<<"error">>, <<"HintNetRequestError">>}]},
        concurrency_test:cast_result(-1, RequestRd),
        Req:respond({200, [], dd_util:encode_json_utf8(RetJsonR)})
  end.



%% Internal API
mod_check(ModName) ->
  case ModName of
	"login" -> ok;
    "get_user_info" -> ok;
	"change_team"->ok;
	"gm" -> ok;
    "activity_game_end" -> ok;
    "buy_goods" -> ok;
    "endless_game_end" -> ok;
    "fast_purchase" -> ok;
    "game_end" -> ok;
    "get_achievement_reward" -> ok;
    "get_activity_reward" -> ok;
    "get_endless_germ" -> ok;
    "get_friend_endless_rank" -> ok;
    "get_login_reward" -> ok;
    "get_mail" -> ok;
    "get_mail_attach" -> ok;
    "get_mass_attach_mail" -> ok;
    "get_mission_reward" -> ok;
    "get_server_endless_rank" -> ok;
    "iap_buy" -> ok;
    "lottery" -> ok;
    "present_friend_strength" -> ok;
    "replace_equipment" -> ok;
    "share_score" -> ok;
    "strengthen" -> ok;
    "sync_user_info" -> ok;
    "world_map_block" -> ok;
    "sync_guide_step" -> ok;
    "get_notice" -> ok;
    "send_feedback" -> ok;
    "get_billno" -> ok;
    "query_order" -> ok;
    "get_user_basic_info" -> ok;
    "replace_inscription" -> ok;
    "compose_inscription" -> ok;
    "enter_tollgate" -> ok;
    "exchange_cdkey" -> ok;
    "advance_inscription" -> ok;
    "update_tower_team" -> ok;
    "tollgate_sweep" -> ok;
	"op_bag" -> ok;
    "op_ladder" -> ok;
    "op_rewardmatch" -> ok;
    OtherRequest ->
      ?FILE_LOG_ERROR("gateway http request error: invalid request => ~p", [OtherRequest]),
      throw({custom, "HintInvalidRequest"})
  end.


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

