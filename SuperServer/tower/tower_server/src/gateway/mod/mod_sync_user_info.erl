%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 九月 2014 下午5:34
%%%-------------------------------------------------------------------
-module(mod_sync_user_info).
-author("zqlt").
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

  SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  SyncData = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("info_data", PostData, undefined), "HintRequestDataError")),

  ?FILE_LOG_DEBUG("sync_user_info => ~p, ~p", [SessionID, SyncData]),
  DataList = gateway_util:decode_sync_data(SyncData),
  if
    length(DataList) =< 0 -> throw({custom, "HintRequestDataError"});
    true -> ok
  end,
  {success, Uin} = gateway_util:get_uin_by_session(SessionID, sync_user_info),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  case rpc:call(CacheNode, cache, sync_user_info, [Uin, DataList]) of
    success ->
      {struct, [{<<"result">>, 0}]};
    {fail, Reason} ->
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.
