%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 十一月 2014 下午4:28
%%%-------------------------------------------------------------------
-module(mod_send_feedback).
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
  FeedBackData = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("feedback", PostData, undefined), "HintRequestDataError")),

  {success, Uin} = gateway_util:get_uin_by_session(SessionID, seed_feedback),

  ?FILE_LOG_DEBUG("send_feedback => ~p, ~p", [Uin, FeedBackData]),
  {success, MailNode} = gateway_util:get_mail_node(),
  rpc:call(MailNode,mail,send_feedback_mail,[FeedBackData,Uin]),
  {struct, [{<<"result">>, 0}]}.