-module(mod_stop_server_time).
-author(j).
-export([req_handle/1]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),

  OperCode =
    try
      dd_util:to_list(proplists:get_value("oper_code", GetData, undefined))
    catch
      _:_ ->
        Ret0 = {struct,[{<<"result">>, -1},{<<"error">>, <<"request param error">>}]},
        throw({custom, Ret0})
    end,

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
  ?FILE_LOG_DEBUG("mod_stop_server_time=> oper_code=~p, uname=~p.", [OperCode, UserName]),
  Stop_time = dd_util:to_list(proplists:get_value("stop_time", GetData, undefined)),
  ?FILE_LOG_DEBUG("stop time = ~p, data = ~p", [Stop_time, GetData]),
  case catch adminserver_cache_proxy:rpc_stopserver_call(Stop_time) of
    success -> %%达到关闭服务器时间
      {struct, [{<<"result">>, 0}]};
    _ ->
      {struct, [{<<"result">>, -1}]}

  end.