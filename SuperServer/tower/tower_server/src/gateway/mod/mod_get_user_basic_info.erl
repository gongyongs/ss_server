%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 十二月 2014 下午2:32
%%%-------------------------------------------------------------------
-module(mod_get_user_basic_info).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
%% API
-export([req_handle/1]).
%% API
-export([]).
req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  gateway_util:signature_check(PostData),

  SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  UserIDs = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("users", PostData, undefined), "HintRequestDataError")),

  ?FILE_LOG_DEBUG("get_user_basic_info => ~p, ~p", [SessionID, UserIDs]),
  {struct, [{<<"id">>, IDList}]} = mochijson2:decode(UserIDs),
  {success, Uin} = gateway_util:get_uin_by_session(SessionID, get_user_basic_info),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  UinList = lists:map(fun(ID) -> dd_util:to_integer(ID) end, IDList),

  if
    length(UinList) =< 0 -> throw({custom, "HintRequestDataError"});
    true -> ok
  end,

  case rpc:call(CacheNode, cache, get_user_basic_info, [Uin, UinList]) of
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
      ?FILE_LOG_ERROR("get_user_basic_info error, reason = ~p", [FailReason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, <<"HintSystemDataError">>}]}
  end.