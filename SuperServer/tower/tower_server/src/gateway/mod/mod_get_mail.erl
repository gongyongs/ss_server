%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 九月 2014 下午3:08
%%%-------------------------------------------------------------------
-module(mod_get_mail).
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

  {success, Uin} = gateway_util:get_uin_by_session(SessionId, get_mail),
  {success, MailNode} = gateway_util:get_mail_node(),

  ?FILE_LOG_DEBUG("get_mail => Uin = ~p : session_id = ~p", [Uin, SessionId]),

  case rpc:call(MailNode, mail, get_mail, [Uin]) of
    {success, {AttachMail, BulletinMail}} ->
      Data =
        {
          struct,
          [
            {<<"friend_mail">>, gateway_util:encode_friend_mail(AttachMail)},
            {<<"system_mail">>, gateway_util:encode_system_mail(AttachMail)},
            {<<"notice_mail">>, gateway_util:encode_notice_mail(BulletinMail)}
          ]
        },
      Json = dd_util:encode_json_utf8(Data),
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, dd_util:to_binary(Json)},
          {<<"sign">>, gateway_util:back_signature(Json)}
        ]
      };
    {fail, Reason} ->
      ?FILE_LOG_ERROR("buy_goods error, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.
