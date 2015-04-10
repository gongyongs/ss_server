%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 十月 2014 下午12:42
%%%-------------------------------------------------------------------
-module(mod_fresh_player).
-author(j).
-export([req_handle/1]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../cache/cache_def.hrl").
%% API

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),

  Action = dd_util:to_list(proplists:get_value("action", GetData, undefined)),

  case Action of
    "fresh_player_count" ->
      ?FILE_LOG_DEBUG("mod_fresh_player=> fresh_player_count.", []),
      Count = adminserver_util:get_online_player_count(),
      {struct, [{<<"result">>, 0},{<<"data">>,Count}]};
     _ ->
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary("No Such Operation")}]}
  end.


