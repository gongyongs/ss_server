%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 十月 2014 下午12:42
%%%-------------------------------------------------------------------
-module(mod_search_player).
-author(j).
-export([req_handle/1]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../cache/cache_def.hrl").
%% API

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),

  Uin = dd_util:to_integer(proplists:get_value("uin", GetData, undefined)),
  Type = dd_util:to_list(proplists:get_value("info_type", GetData, undefined)),
  ?FILE_LOG_DEBUG("mod_search_player=> uin=~p, type=~p", [Uin, Type]),

  {success, CacheNode} = adminserver_util:get_cache_node(Uin),

  case rpc:call(CacheNode, cache_admin, get_user_info, [Uin, [Type]]) of
    {success, ValueItem} ->
      ?FILE_LOG_DEBUG("mod_search_player=>cache=>get_user_info=> success",[]),
      [ValueItem_con] = ValueItem ,
      Val =
            case ValueItem_con of
              {"player", Player_Data} -> Player_Data;
              {"backpack", Backpack} ->
                adminserver_util:encode_json_backpack(Backpack);
              {"tower", Tower} ->                                       %塔
                adminserver_util:encode_json_tower(Tower)
            end,
      {struct, [{<<"result">>, 0}, {<<"data">>, Val}]};
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("mod_search_player=>cache=>get_user_info=> fail",[]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.
