%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十二月 2014 下午4:36
%%%-------------------------------------------------------------------
-module(mod_post_error_msg).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
%% API
-export([req_handle/1]).


req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST),"request method error"),
  PostData = Req:parse_post(),

  SessionId = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  Version = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("version", PostData, undefined), "HintRequestDataError")),
  Message = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("message", PostData, undefined), "HintRequestDataError")),
  TraceBack = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("trace_back", PostData, undefined), "HintRequestDataError")),

  Uin =
  case SessionId of
    "0" -> -1;
    _ ->
      {success, Uid} = gateway_util:get_uin_by_session(SessionId, post_error_msg),
      Uid
  end,

  ?FILE_LOG_DEBUG("post_error message => Uin = ~p : session_id = ~p", [Uin, SessionId]),

  Path = os:getenv("SASL_LOG_PATH"),
  {Date, _} = calendar:local_time(),
  FileName = Path ++ "/" ++ dd_util:time_format_without_hms(Date) ++ ".error",

  {ok, F} = file:open(FileName, [append]),
  io:format(F, "~p\t~p\t~p\t~p\t~n", [Uin, Version, Message, TraceBack]),
  file:close(F),
  {
    struct,
    [
      {<<"result">>, 0}
    ]
  }.

