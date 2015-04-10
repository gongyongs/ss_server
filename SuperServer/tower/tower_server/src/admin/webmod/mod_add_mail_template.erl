%%%-------------------------------------------------------------------
%%% @author 95
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 六月 2014 下午4:50
%%%-------------------------------------------------------------------
-module(mod_add_mail_template).
-author("95").
-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),
  ?FILE_LOG_DEBUG("getdata= ~p.", [GetData]),
  {TemplateTitle, TemplateContent, TemplateTag, TemplateParamNum} =
    try
      {
        dd_util:to_list(proplists:get_value("template_title", GetData, undefined)),
        dd_util:to_list(proplists:get_value("template_content", GetData, undefined)),
        dd_util:to_list(proplists:get_value("template_tag", GetData, undefined)),     %%tag即为索引
        dd_util:to_integer(proplists:get_value("param_num",GetData, undefined))
      }
    catch
      _:_ ->
        ?FILE_LOG_DEBUG("param error", []),
        throw({custom, "Param error"})
    end,


  ?FILE_LOG_DEBUG("mod_add_mail_template", []),
  ?FILE_LOG_DEBUG("title= ~p, content = ~p~n.", [TemplateTitle,TemplateContent]),
  case catch adminserver_cache_proxy:rpc_mail_call(add_mail_template, [TemplateTag, TemplateTitle,TemplateContent,TemplateParamNum]) of
    {success, TemplateID} ->
      ?FILE_LOG_DEBUG("success, TemplateID = ~p~n.", [TemplateID]),
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"template_id">>, dd_util:to_binary(TemplateID)}
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