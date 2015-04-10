%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 一月 2015 下午2:21
%%%-------------------------------------------------------------------
-module(mod_enter_tollgate).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST),"request method error"),
  PostData = Req:parse_post(),

  gateway_util:signature_check(PostData),

  SessionId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  TollgateID = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("tollgate_id", PostData, undefined), "HintRequestDataError")),
  {success, Uin} = gateway_util:get_uin_by_session(SessionId, enter_tollgate),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  ?FILE_LOG_DEBUG("enter_tollgate =>  session_id = ~p, TollgateID = ~p, Uin = ~p", [SessionId, TollgateID, Uin]),

  case rpc:call(CacheNode, cache, enter_tollgate, [Uin, TollgateID]) of
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
      ?FILE_LOG_ERROR("enter_tollgate error, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.
