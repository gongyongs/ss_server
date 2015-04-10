%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 八月 2014 下午2:22
%%%-------------------------------------------------------------------
-module(mod_get_user_info).
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

  ?FILE_LOG_DEBUG("get user info post data = ~p", [PostData]),

  SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  GetInfoType = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("info_type", PostData, undefined), "HintRequestDataError")),

  ?FILE_LOG_DEBUG("get_user_info => ~p, ~p", [SessionID, GetInfoType]),
  {struct, [{<<"type">>, TypeList}]} = mochijson2:decode(GetInfoType),
  {success, Uin} = gateway_util:get_uin_by_session(SessionID, get_user_info),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  NewList = lists:map(fun(Type) -> dd_util:to_list(Type) end, TypeList),
  case rpc:call(CacheNode, cache, get_user_info, [Uin, NewList]) of
    {success, DataBin} ->
      {struct,
        [
          {<<"result">>, 0},
          {<<"data">>, DataBin},
          {<<"sign">>, gateway_util:back_signature(DataBin)}
        ]
      };
    {fail, Reason} ->
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.


