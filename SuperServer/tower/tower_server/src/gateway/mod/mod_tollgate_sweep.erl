%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 一月 2015 下午2:07
%%%-------------------------------------------------------------------
-module(mod_tollgate_sweep).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),

  gateway_util:signature_check(PostData),

  SessionId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  TollgateID = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("tid", PostData, undefined), "HintRequestDataError")),
  SweepTimes = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("count", PostData, undefined), "HintRequestDataError")),

  ?FILE_LOG_DEBUG("tollgate_sweep => ~p, ~p, ~p", [SessionId, TollgateID, SweepTimes]),
  gateway_util:check_tollgate_mode(TollgateID),
  {success, Uin} = gateway_util:get_uin_by_session(SessionId, tollgate_sweep),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  ?FILE_LOG_DEBUG("tollgate_sweep => uin = ~p, id = ~p, times = ~p", [Uin, TollgateID, SweepTimes]),

  case rpc:call(CacheNode, cache, tollgate_sweep, [Uin, TollgateID, SweepTimes]) of
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

