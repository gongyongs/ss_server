%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 十二月 2014 下午3:22
%%%-------------------------------------------------------------------
-module(mod_query_all_notice).
-author("zqlt").
-include("../adminserver.hrl").
-include("../../cache/cache_def.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([req_handle/1]).

result_to_erl(NoticeList) ->
  L =
    lists:map(
      fun(Notice) ->
        {
          struct,
          [
            {<<"id">>, Notice#notice_rd.notice_id},
            {<<"title">>, dd_util:to_binary(Notice#notice_rd.notice_title)},
            {<<"content">>, dd_util:to_binary(Notice#notice_rd.notice_detail)},
            {<<"date">>, dd_util:to_binary(Notice#notice_rd.notice_date)},
            {<<"url">>, dd_util:to_binary(Notice#notice_rd.notice_pic_url)},
            {<<"sign">>, dd_util:to_binary(Notice#notice_rd.notice_sign)}
          ]
        }
      end, NoticeList),
  {struct, [{<<"notice">>, L}]}.


req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  _GetData = Req:parse_post(),

  ?FILE_LOG_DEBUG("mod_query_all_notice", []),

  case catch adminserver_db:query_all_notice() of
    {success, NoticeList} ->
      ResultData = result_to_erl(NoticeList),
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, ResultData}
        ]
      };
    {fail, Reason} ->
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"error">>, dd_util:to_binary(Reason)}
        ]
      }
  end.