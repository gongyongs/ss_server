%%%-------------------------------------------------------------------
%%% @author 95
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 六月 2014 下午4:21
%%%-------------------------------------------------------------------
-module(mod_query_all_mail_template).
-author("95").
-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../mail/mail.hrl").
%% API
-export([req_handle/1]).

result_to_erl(TemplateL) ->
		lists:map(
			fun(Element) ->
				{struct,
          [
            {<<"id">>, dd_util:to_binary(Element#template_mail.template_id)},
            {<<"tag">>, dd_util:to_binary(Element#template_mail.template_tag)},
            {<<"title">>, dd_util:to_binary(Element#template_mail.template_title)},
            {<<"content">>, dd_util:to_binary(Element#template_mail.template_content)},
            {<<"param_num">>, Element#template_mail.template_content_parm_len}
          ]
        }
			end, TemplateL).

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  _GetData = Req:parse_post(),

  ?FILE_LOG_DEBUG("mod_query_all_mail_template", []),

  case catch adminserver_cache_proxy:rpc_mail_call(get_mail_template, []) of
    {success, TemplateList} ->
      ?FILE_LOG_DEBUG("TemplateList is ~p.", [TemplateList]),
      JsonList = result_to_erl(TemplateList),
      TemplateJson = {struct, [{<<"template">>, JsonList}]},
      {
          struct,
          [
             {<<"result">>, 0},
             {<<"data">>, TemplateJson}
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