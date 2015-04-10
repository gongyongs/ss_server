%%%-------------------------------------------------------------------
%%% @author 95
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 六月 2014 上午11:54
%%%-------------------------------------------------------------------
-module(mod_add_attach_mail).
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

  ?FILE_LOG_DEBUG("mod_add_attach_mail=> getdata =  ~p~n", [GetData]),
  {TempleID, UinL, SourceID, Type, Av1, Av2, ParamJson, Term} =
  	try
  		{
  			dd_util:to_list(proplists:get_value("template_id", GetData, undefined)),  %模板ID
  			dd_util:to_list(proplists:get_value("uins", GetData, undefined)),            %收件人ID
        dd_util:to_integer(proplists:get_value("source_id", GetData, undefined)),  %发件人ID
  			dd_util:to_integer(proplists:get_value("type", GetData, undefined)),
  			dd_util:to_list(proplists:get_value("av1",GetData,undefined)),    %道具ID
  			dd_util:to_integer(proplists:get_value("av2",GetData, undefined)),   %道具数量
  			dd_util:to_list(proplists:get_value("param",GetData, undefined)),    %邮件内容
        dd_util:to_integer(proplists:get_value("term", GetData, undefined)) %%期限
  		}
  	catch
  		_:_ ->
  			Ret0 = {struct, [{<<"result">>, -1}, {<<"error">>, <<"request param error">>}]},
  			throw({custom,Ret0})		
  	end,

  ?FILE_LOG_DEBUG("mod_add_attach_mail=> template_id=~p, uins = ~p, source_id = ~p, type = ~p, attach_id = ~p, attach_count = ~p, param = ~p", [TempleID, UinL, SourceID, Type, Av1, Av2, ParamJson]),

  NAv1 =
    case Type of
      4 -> Av1;
      _ -> ""
    end,

  UinList = lists:map(fun(Item) -> dd_util:to_integer(Item) end, string:tokens(UinL, ",")),

  {success, ParamList}  = decode_param(ParamJson),
   case catch adminserver_cache_proxy:rpc_mail_call(send_mass_attach_mail, [SourceID, UinList, TempleID, ParamList, {Type, NAv1, Av2}, Term]) of
     {success, {SuccessList, FailList}} ->
       ?FILE_LOG_DEBUG("send mass mail success, success list = ~p, fail list = ~p", [SuccessList, FailList]),
       {
         struct,
         [
           {<<"result">>, 0},
           {<<"success_list">>, SuccessList},
           {<<"fail_list">>, FailList}
         ]
       };
     Other ->
       ?FILE_LOG_DEBUG("send mass mail fail, reason = ~p", [Other]),
       {
          struct,
         [
           {<<"result">>, -1}
         ]
       }
   end.

