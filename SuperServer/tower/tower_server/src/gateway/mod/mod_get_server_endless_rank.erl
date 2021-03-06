%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 九月 2014 下午4:35
%%%-------------------------------------------------------------------
-module(mod_get_server_endless_rank).
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

  {success, Uin} = gateway_util:get_uin_by_session(SessionID, get_server_endless_rank),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  case rpc:call(CacheNode, cache, get_server_endless_rank, [Uin]) of
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
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.
