%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2014 17:46
%%%-------------------------------------------------------------------
-module(mod_change_game_model_function).
-author(j).
-export([req_handle/1]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API


req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  PostData = Req:parse_post(),

  OperCode =
    try
      dd_util:to_list(proplists:get_value("oper_code", PostData, undefined))
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

  ?FILE_LOG_DEBUG("mod_change_game_model_function=> oper_code=~p, uname=~p.", [OperCode, UserName]),

 Action = list_to_atom(dd_util:to_list(proplists:get_value("function", PostData, undefined))),

  ?FILE_LOG_INFO("Action = ~p",[Action]),
  case adminserver_db:change_model_state(Action) of           %改变功能状态
    success->
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





