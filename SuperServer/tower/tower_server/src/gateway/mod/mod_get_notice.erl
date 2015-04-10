%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 十一�?2014 下午3:49
%%%-------------------------------------------------------------------
-module(mod_get_notice).
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

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, get_notice),

  ?FILE_LOG_DEBUG("get_notice => Uin = ~p : session_id = ~p", [Uin, SessionId]),

  NoticeList =
  case gateway_db:get_notice_rd(Uin) of
    {success, Notice_rdList}->Notice_rdList;
    {fail,Reason} ->
      ?FILE_LOG_DEBUG("get_notice_rd fail,Reason is ~p",[Reason]),
      []
  end,
  JsonData = gateway_util:encode_json_notice(NoticeList),
  Data = {struct, [{<<"notice_info">>, JsonData}]},
  Json = dd_util:encode_json_utf8(Data),
  {
    struct,
    [
      {<<"result">>, 0},
      {<<"data">>, dd_util:to_binary(Json)},
      {<<"sign">>, gateway_util:back_signature(Json)}
    ]
  }.


