%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2014 17:46
%%%-------------------------------------------------------------------
-module(mod_query_shop_item).
-author(j).
-export([req_handle/1]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../csv.hrl").
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
  ?FILE_LOG_DEBUG("mod_add_shop_item => oper_code=~p, uname=~p.", [OperCode, UserName]),

  case adminserver_db:query_shop_config() of           %查询商城物品
    {success, {CsvConfig, DbConfig}}->
      CsvConfig_Json = adminserver_util:encode_shop_item(CsvConfig),
      DbConfig_Json = adminserver_util:encode_shop_item(DbConfig),
      CsvConfig_Json1 = dd_util:encode_json_utf8(CsvConfig_Json),
      DbConfig_Json1 = dd_util:encode_json_utf8(DbConfig_Json),
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"CsvConfig">>, dd_util:to_binary(CsvConfig_Json1)},
          {<<"DbConfig">>, dd_util:to_binary(DbConfig_Json1)}
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


