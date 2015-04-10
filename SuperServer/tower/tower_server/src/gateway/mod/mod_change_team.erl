%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 一月 2015 下午5:06
%%%-------------------------------------------------------------------
-module(mod_change_team).
-author("zqlt").

-include("../../../deps/file_log/include/file_log.hrl").
-include("../../cache/cache_def.hrl").
-include("../gateway.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check(Req:get(method) =:= ?POST, "request mothod error"),
  PostData = Req:parse_post(),
  gateway_util:signature_check(PostData),

  SessionId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  SuperTeamJson = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("super_team", PostData, undefined), "HintRequestDataError")),
  ?FILE_LOG_DEBUG("now in change super team => session = ~p, team = ~p", [SessionId, SuperTeamJson]),

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, change_team),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  SuperTeam = gateway_util:decode_super_team_json(SuperTeamJson),

  case rpc:call(CacheNode, cache, change_team, [Uin, SuperTeam]) of
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
      ?FILE_LOG_ERROR("change team proc error, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.