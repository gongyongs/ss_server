%%%-------------------------------------------------------------------
%%% @author 95
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 六月 2014 上午11:54
%%%-------------------------------------------------------------------
-module(mod_distribution_gift).
-author("95").
-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../mail/mail.hrl").

%% API
-export([req_handle/1]).

decode_param(ParamListJson) ->
  DataBody =
    case mochijson2:decode(ParamListJson) of
      [{struct, TmpDataBody}] -> {success, TmpDataBody};
      {struct, TmpDataBody} ->  {success, TmpDataBody};
      _OtherData ->  {fail, "json error"}
    end,
  ?FILE_LOG_DEBUG("params = ~p data_bady=~p.", [ParamListJson, DataBody]),
  DataBody.

add_mail(MailInfo) ->
  {SrcID, Uin, Tid, Params, Attach} = MailInfo,
  case catch adminserver_cache_proxy:rpc_mail_call(add_attach_mail, [SrcID, dd_util:to_integer(Uin), Tid, Params, Attach,7]) of    %7表示邮件有效期限
    {success, AttachMail} ->
      ?FILE_LOG_DEBUG("mod_add_attach_mail=>add_mail=>success Mail = ~p d", [AttachMail]),
      {success, Uin, AttachMail#attach_mail.mail_id};
    {fail, _} ->
      {fail, Uin, "system error"}
  end.

cnt_success_fail_mail(L) ->
  cnt_success_fail_mail(L, [], []).

cnt_success_fail_mail([H | T], SuccessList, FailList) ->
  case H of
    {success, _, _} ->
      cnt_success_fail_mail(T, [H | SuccessList], FailList);
    {fail, _, _} ->
      cnt_success_fail_mail(T, SuccessList, [H | FailList])
  end;
cnt_success_fail_mail([], SuccessList, FailList) ->
  {SuccessList, FailList}.

result_to_erl(S, F) ->
  TmpS =
    lists:map(
      fun(SElement) ->
        {success, Uin, Mid} = SElement,
        {dd_util:to_binary(Uin), {struct, [{<<"mailID">>, Mid}]}}
      end, S),
  JsonS = {<<"success">>, {struct, TmpS}},
  TmpF =
    lists:map(
      fun(FElement) ->
        {fail, Uin, Reason} = FElement,
        {dd_util:to_binary(Uin), {struct, [{<<"reason">>, dd_util:to_binary(Reason)}]}}
      end, F),
  JsonF = {<<"fail">>, {struct, TmpF}},
  {struct, [JsonS, JsonF]}.



req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),
  ?FILE_LOG_DEBUG(" go to mod_distribution_gift ",[]),

  {TempleID, Uin_factor, SourceID, OperCode, Type, Av1, Av2, ParamJson} =
    try
      {
        dd_util:to_list(proplists:get_value("template_id", GetData, undefined)),  %模板ID
        dd_util:to_integer(proplists:get_value("uin_factor", GetData, undefined)),            %收件人ID
        dd_util:to_integer(proplists:get_value("source_id", GetData, undefined)),  %发件人ID
        dd_util:to_list(proplists:get_value("oper_code",GetData, undefined)),
        dd_util:to_integer(proplists:get_value("type", GetData, undefined)),
        dd_util:to_list(proplists:get_value("av1",GetData,undefined)),    %道具ID
        dd_util:to_integer(proplists:get_value("av2",GetData, undefined)),   %道具数量
        dd_util:to_list(proplists:get_value("params",GetData, undefined))    %邮件内容
      }
    catch
      _:_ ->
        Ret0 = {struct, [{<<"result">>, -1}, {<<"error">>, <<"request param error">>}]},
        throw({custom,Ret0})
    end,

  ?FILE_LOG_DEBUG("Uin_factor is ~p",[Uin_factor]),

  {success, UinList} = adminserver_db:get_uins_above_pay(Uin_factor),  %获取充值金额以上的Uin
  ?FILE_LOG_DEBUG("UinList is ~p",[UinList]),

  UserName =
    case adminserver_cache_proxy:get_admin(OperCode) of
      fail ->
        Ret1 = {struct,[{<<"result">>, -1},{<<"error">>, <<"Illegal Operation, Please login again">>}]},
        throw({custom, Ret1});
      {success, AdminUName} -> AdminUName;
      _Other ->
        Ret2 = {struct,[{<<"result">>, -1},{<<"error">>, <<"system error">>}]},
        throw({custom, Ret2})
    end,
  ?FILE_LOG_DEBUG("mod_add_attach_mail=> oper_code=~p, uname=~p.", [OperCode, UserName]),

  case decode_param(ParamJson) of
    {success, ParamList} ->
      ParamL = [dd_util:to_list(Val) || {_,Val} <- ParamList],
      MailList = [{SourceID, Uin, TempleID,ParamL,{Type, Av1, Av2}} || Uin <- UinList],
      ResultList = lists:map(fun add_mail/1, MailList),
      ?FILE_LOG_DEBUG("mod_add_attach_mail=> ResultList=~p", [ResultList]),
      {SList, FList} = cnt_success_fail_mail(ResultList),
      T = result_to_erl(SList, FList),
      Json = dd_util:encode_json_utf8(T),
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, dd_util:to_binary(Json)}
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
