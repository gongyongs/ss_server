%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 十二月 2014 下午2:17
%%%-------------------------------------------------------------------
-module(mod_notice_manager).
-author("zqlt").

-export([req_handle/1]).

-include("../adminserver.hrl").
-include("../../cache/cache_def.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  PostData = Req:parse_post(),

  Action = list_to_atom(dd_util:to_list(proplists:get_value("action", PostData, undefined))),
  Data = dd_util:to_list(proplists:get_value("data", PostData, undefined)),
  DecodeData = decode_data(Action, Data),

  ?FILE_LOG_DEBUG("notice_manager => action = ~p, data = ~p", [Action, DecodeData]),

  case adminserver_db:notice_manager(Action, DecodeData) of
    success ->
      {
        struct,
        [
          {<<"result">>, 0}
        ]
      };
    Other ->
      ?FILE_LOG_DEBUG("notice_manager => action = ~p, param = ~p, reason = ~p", [Action, DecodeData, Other]),
      {
        struct,
        [
          {<<"result">>, -1}
        ]
      }
  end.

decode_data(add_notice, Data) ->
  {struct, JsonList} = mochijson2:decode(Data),
  #notice_rd{
    notice_id = 0,
    notice_date = dd_util:to_list(adminserver_util:get_json_value(<<"notice_date">>, JsonList)),
    notice_title = dd_util:to_list(adminserver_util:get_json_value(<<"notice_title">>, JsonList)),
    notice_detail = dd_util:to_list(adminserver_util:get_json_value(<<"notice_content">>, JsonList)),
    notice_pic_url = dd_util:to_list(adminserver_util:get_json_value(<<"notice_url">>, JsonList)),
    notice_sign = dd_util:to_list(adminserver_util:get_json_value(<<"notice_sign">>, JsonList))
  };
decode_data(modify_notice, Data) ->
  {struct, JsonList} = mochijson2:decode(Data),
  #notice_rd{
    notice_id = dd_util:to_integer(adminserver_util:get_json_value(<<"notice_id">>, JsonList)),
    notice_date = dd_util:to_list(adminserver_util:get_json_value(<<"notice_date">>, JsonList)),
    notice_title = dd_util:to_list(adminserver_util:get_json_value(<<"notice_title">>, JsonList)),
    notice_detail = dd_util:to_list(adminserver_util:get_json_value(<<"notice_content">>, JsonList)),
    notice_pic_url = dd_util:to_list(adminserver_util:get_json_value(<<"notice_url">>, JsonList)),
    notice_sign = dd_util:to_list(adminserver_util:get_json_value(<<"notice_sign">>, JsonList))
  };
decode_data(delete_notice, Data) ->
  dd_util:to_integer(Data);
decode_data(_, _) ->
  throw({custom, "unknow type"}).
