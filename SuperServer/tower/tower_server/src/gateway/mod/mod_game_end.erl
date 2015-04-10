%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 八月 2014 下午3:54
%%%-------------------------------------------------------------------
-module(mod_game_end).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST),"request method error"),
  PostData = Req:parse_post(),
  ?FILE_LOG_DEBUG("game end post data = ~p", [PostData]),
  SessionId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  GameData = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("game_data", PostData, undefined), "HintRequestDataError")),

  gateway_util:signature_check(PostData),

  ?FILE_LOG_DEBUG("game_end => ~p, ~p", [SessionId, GameData]),
  {struct, DataList} = mochijson2:decode(GameData),
  {success, GameEndData} = gateway_util:decode_game_end_data(DataList),
  gateway_util:check_tollgate_mode(GameEndData#game_end.tollgate_id),
  {success, Uin} = gateway_util:get_uin_by_session(SessionId, {game_end, GameEndData#game_end.data_statistics#game_end_statistics.total_kill_all_monster}),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  ?FILE_LOG_DEBUG("game_end => uin = ~p, game_end_data = ~p", [Uin, GameEndData]),

  case rpc:call(CacheNode, cache, game_end, [Uin, GameEndData]) of
    {success, DataBin} ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, DataBin},
          {<<"sign">>, gateway_util:back_signature(DataBin)}
        ]
      };
    {fail, Reason} ->
      ?FILE_LOG_ERROR("game end proc error, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.






