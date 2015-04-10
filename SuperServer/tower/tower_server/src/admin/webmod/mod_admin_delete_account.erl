%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十一月 2014 上午11:35
%%%-------------------------------------------------------------------
-module(mod_admin_delete_account).
-author("zqlt").

%% API
-export([req_handle/1]).
-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../cache/cache_def.hrl").
%% API

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),
  Uin = dd_util:to_integer(proplists:get_value("uin", GetData, undefined)),
  Data = dd_util:to_list(proplists:get_value("data", GetData, undefined)),
  Action = list_to_atom(dd_util:to_list(proplists:get_value("action", GetData, undefined))),

  ?FILE_LOG_DEBUG("admin_delete_account => uin = ~p, action = ~p, data = ~p", [Uin, Action, Data]),

  DecodeData = decode_data(Action, Data),
  {success, CacheNode} = adminserver_util:get_cache_node(Uin),

  case rpc:call(CacheNode, cache_admin, del_account, [Uin, Action, DecodeData]) of
    success ->
      {struct, [{<<"result">>, 0}]};
    Other ->
      ?FILE_LOG_DEBUG("admin_delete_account => uin = ~p, action = ~p, fail reason = ~p", [Uin, Action, Other]),
      {struct, [{<<"result">>, -1}]}
  end.

decode_data(delete_player, Data) -> Data;
decode_data(clear_player_data, Data) -> Data;
decode_data(_, _) -> throw({custom, "ErrorActionType"}).