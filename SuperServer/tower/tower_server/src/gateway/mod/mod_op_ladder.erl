%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 三月 2015 14:14
%%%-------------------------------------------------------------------
-module(mod_op_ladder).
-author("zqlt").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
-include("../../../deps/file_log/include/file_log.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([req_handle/1]).
-export([]).

req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  gateway_util:signature_check(PostData),

  ?FILE_LOG_DEBUG("op ladder post data = ~p", [PostData]),

  SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  OP = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("op", PostData, undefined), "HintRequestDataError")),
  Params = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("param", PostData, undefined), "HintRequestDataError")),


  ?FILE_LOG_DEBUG("op_ladder => ~p, ~p", [SessionID, OP]),
  {success, Uin} = gateway_util:get_uin_by_session(SessionID, op_ladder),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  _Ip = Req:get(peer),

  RPCRESULT = rpc:call(CacheNode, cache, op_ladder, [Uin,OP,Params]),

  %case rpc:call(CacheNode, cache, op_ladder, [Uin,OP,Params]) of
  case RPCRESULT of
    {success, DataBin} ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, DataBin},
          {<<"sign">>, gateway_util:back_signature(DataBin)}
        ]
      };
    FailReason ->
      ?FILE_LOG_ERROR("op_ladder error, reason = ~p", [FailReason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, <<"HintSystemDataError">>}]}
  end.
