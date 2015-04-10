%%%-------------------------------------------------------------------
%%% @author 95
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 六月 2014 下午6:38
%%%-------------------------------------------------------------------
-module(mod_add_bulletin_mail).
-author("95").
-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../mail/mail.hrl").
%% API
-export([req_handle/1]).

decode_param(ParamListJson) ->
  {struct, JsonList} = mochijson2:decode(ParamListJson),
  ParamList = adminserver_util:get_json_value(<<"param">>, JsonList),
  {success, lists:map(fun(Item) -> dd_util:to_list(Item) end, ParamList)}.

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),
  ?FILE_LOG_DEBUG("post data =~p", [GetData]),
  {TemplateTitle, TemplateID, ParamListJson, MailTerm} =
    try
      {
        dd_util:to_list(proplists:get_value("template_title", GetData, undefined)),
        dd_util:to_list(proplists:get_value("template_id", GetData, undefined)),
        dd_util:to_list(proplists:get_value("param", GetData, undefined)),
        dd_util:to_integer(proplists:get_value("term", GetData, undefined))
      }
    catch
      _:_ ->
        Ret0 = {struct,[{<<"result">>, -1},{<<"error">>, <<"request param error">>}]},
        throw({custom, Ret0})
    end,

  ?FILE_LOG_DEBUG("mod_add_bulletin_mail, [~p, ~p, ~p, ~p]", [TemplateTitle, TemplateID, ParamListJson, MailTerm]),

  case decode_param(ParamListJson) of
    {success, ParamList} ->
      case catch adminserver_cache_proxy:rpc_mail_call(add_bulletin_mail, [TemplateID, ParamList, MailTerm]) of
        {success, MailId} ->
         {
            struct,
            [
              {<<"result">>, 0},
              {<<"template_id">>, dd_util:to_binary(MailId#bulletin_mail.mail_id)}
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
      end;
    {fail, Reason} ->
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"error">>, dd_util:to_binary(Reason)}
        ]
      }
  end.