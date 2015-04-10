%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 九月 2014 下午5:56
%%%-------------------------------------------------------------------
-module(mod_get_mail_attach).
-author("zqlt").

-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  gateway_util:signature_check(PostData),

  SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  MailID = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("mail_id", PostData,undefied), "HintRequestDataError")),

  {success, Uin} = gateway_util:get_uin_by_session(SessionID, get_mail_attach),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  ?FILE_LOG_DEBUG("get_mail_attach => uin = ~p, session = ~p, mail_id =  ~p", [Uin, SessionID, MailID]),

  case rpc:call(CacheNode, cache, get_mail_attach, [Uin, MailID]) of
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
      ?FILE_LOG_ERROR("get_login_reward error, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.