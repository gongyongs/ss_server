%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 八月 2014 下午7:27
%%%-------------------------------------------------------------------
-module(mod_get_achievement_reward).
-author("zqlt").

-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").

-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),

  gateway_util:signature_check(PostData),

  SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  GroupID = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("group_id", PostData,undefied), "HintRequestDataError")),
  AchievementID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("achievement_id", PostData,undefied), "HintRequestDataError")),

  ?FILE_LOG_DEBUG("get_achievement_reward => session id = ~p, group id = ~p, achievement id = ~p", [SessionID, GroupID, AchievementID]),

  {success, Uin} = gateway_util:get_uin_by_session(SessionID, get_achievement_reward),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  case rpc:call(CacheNode, cache, get_achievement_reward, [Uin, GroupID, AchievementID]) of
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
      ?FILE_LOG_ERROR("get_login_reward error, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.