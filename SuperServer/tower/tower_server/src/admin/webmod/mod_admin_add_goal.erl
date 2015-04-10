%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2014 17:46
%%%-------------------------------------------------------------------
-module(mod_admin_add_goal).
-author(j).
-export([req_handle/1]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API


req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  PostData = Req:parse_post(),

  ?FILE_LOG_DEBUG("post data = ~p", [PostData]),

  Uin = dd_util:to_list(proplists:get_value("uin", PostData, undefined)),
  UinList = string:tokens(Uin, ","),
  UinList1=lists:map(fun(X)->list_to_integer(X) end,UinList),

  Action = list_to_atom(dd_util:to_list(proplists:get_value("action", PostData, undefined))),
  JsonStr = dd_util:to_list(proplists:get_value("data", PostData, undefined)),
  Oper = dd_util:to_integer(proplists:get_value("oper", PostData, undefined)),            % 1增加  0减少

  {struct,Data} = mochijson2:decode(JsonStr),

  ParamList= [{Action,Uin1,Data,Oper} || Uin1 <- UinList1],
  ResultList = lists:map(fun handleAction/1, ParamList),     %获得结果组合
  Result = lists:all(fun(X) -> X=:=success end,ResultList),       %都返回true则返回true

  case Result of
    true ->
      {
        struct,
        [
          {<<"result">>, 0}
        ]
      };
    _ ->
      {
        struct,
        [
          {<<"result">>, -1}
        ]
      }
  end.


handleAction({Action,Uin,Data,Oper})->
  ?FILE_LOG_DEBUG("web request ~p ~p ~p:", [Action, Uin,Data]),
  {success,Node} = adminserver_util:get_cache_node(Uin),
  Data2 = rpc:call(Node, cache_admin, handler_action,[Action,Uin,Data,Oper]),      %调用cache_admin的handler_action功能

  case Data2 of
    success ->success;     %成功返回1
    {fail,Reason}->{fail,Reason}          %不成功返回0
  end.

