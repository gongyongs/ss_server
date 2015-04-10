%%%-------------------------------------------------------------------
%%% @author 95
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 六月 2014 下午5:50
%%%-------------------------------------------------------------------
-module(mod_query_mail_template).

%% API
-export([]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../mail/mail.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),

  TemplateID = dd_util:to_list(proplists:get_value("template_id", GetData, undefined)),

  ?FILE_LOG_DEBUG("mod_query_user_property, template id = ~p", [TemplateID]),

  case catch adminserver_cache_proxy:rpc_mail_call(get_mail_template, []) of
    {success, TemplateList} ->
      {
         case [Item || #template_mail{template_id = ID} = Item <- TemplateList, ID =:= TemplateID] of
                [] ->
                  struct,
                        [
                           {<<"result">>, -2},
                           {<<"error">>,<<"not exist">>}
                        ];
                [Template] ->
                  struct,
                    [
                      {<<"result">>, 0},
                      {<<"template_title">>,dd_util:to_binary(Template#template_mail.template_title)},
                      {<<"template_content">>,dd_util:to_binary(Template#template_mail.template_content)},
                      {<<"param_num">>, dd_util:to_binary(Template#template_mail.template_content_parm_len)}
                    ];
                _ ->
                  [
                    {<<"result">>, -2},
                    {<<"error">>,<<"template mail id error">>}
                  ]
          end
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
