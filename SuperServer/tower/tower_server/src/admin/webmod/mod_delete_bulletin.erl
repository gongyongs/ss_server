%%%-------------------------------------------------------------------
%%% @author 95
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 六月 2014 下午9:01
%%%-------------------------------------------------------------------
-module(mod_delete_bulletin).
-author("95").
-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),

  MailID = dd_util:to_integer(proplists:get_value("mail_id", GetData, undefined)),

  ?FILE_LOG_DEBUG("mod_delete_bulletin=> mail_id = ~p", [MailID]),

  case catch adminserver_cache_proxy:rpc_mail_call(del_bulletin_mail, [MailID]) of
    {success, _} ->
      {
        struct,
        [
          {<<"result">>, 0}
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

