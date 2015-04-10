%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 十一月 2014 下午1:19
%%%-------------------------------------------------------------------
-module(mod_pp).
-author("zqlt").
-include("mod_pp.hrl").
-include("../../http_proc.hrl").
-include("../../../../deps/file_log/include/file_log.hrl").


%% API
-export([
  generate_orders/3,
  query_order/2,
  call_back_handle/1
]).

query_order(UserID, BillNo) ->
  ?FILE_LOG_DEBUG("query order, user = ~p, bill no = ~p", [UserID, BillNo]),
  mod_pp_ms:query_order(UserID, BillNo).

generate_orders(UserID, GoodsID, Uin) when is_list(UserID) andalso is_list(GoodsID) andalso is_integer(Uin) ->
  ?FILE_LOG_DEBUG("generate_order => userid = ~p, goodid = ~p, uin = ~p", [UserID, GoodsID, Uin]),
  mod_pp_ms:generate_orders(UserID, GoodsID, Uin).

call_back_handle(Req) ->
  PostData = Req:parse_post(),
  try
    OrderID = dd_util:to_integer(http_proc_util:undefined_check(proplists:get_value("order_id", PostData, undefined), "order_id not exist")),
    BillNo = dd_util:to_list(http_proc_util:undefined_check(proplists:get_value("billno", PostData, undefined), "bill_no not exist")),
    AccountID = dd_util:to_list(http_proc_util:undefined_check(proplists:get_value("account", PostData, undefined), "account not exist")),
    AmountPPCoin = dd_util:to_list(http_proc_util:undefined_check(proplists:get_value("amount", PostData, undefined), "amount not exist")),
    Status = dd_util:to_integer(http_proc_util:undefined_check(proplists:get_value("status", PostData, undefined), "status not exist")),
    AppId = dd_util:to_integer(http_proc_util:undefined_check(proplists:get_value("app_id", PostData, undefined), "app id not exist")),
    Sign = dd_util:to_list(http_proc_util:undefined_check(proplists:get_value("sign", PostData, undefined), "sign not exist")),
    {success, Key} = mod_pp_ms:read_public_key(),
    ?FILE_LOG_DEBUG("order_id = ~p, bill = ~p, account id = ~p, amount = ~p, status = ~p, appid = ~p, sign = ~p", [OrderID, BillNo, AccountID, AmountPPCoin, Status, AppId, Sign]),
    SignResult = verify_sign(http_proc_util:base64_decode(Sign), Key, {OrderID, BillNo, AccountID, AmountPPCoin, Status, AppId}),
    mod_pp_ms:complete_orders(OrderID, BillNo, AccountID, dd_util:to_list(AmountPPCoin), AppId, Status, SignResult)
  catch
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p, postdata = ~p", [What, Type, erlang:get_stacktrace(), PostData]),
      <<"success">>
  end.


verify_sign(Sign, Key, {OrderID, BillNo, AccountID, AmountPPCoin, Status, AppId}) ->
  try
    Bin = public_key:decrypt_public(Sign, Key),
    {struct, DataList} = mochijson2:decode(Bin),
    OrderID =:= dd_util:to_integer(http_proc_util:get_json_value(<<"order_id">>, DataList)) andalso
    BillNo =:= dd_util:to_list(http_proc_util:get_json_value(<<"billno">>, DataList)) andalso
    AccountID =:= dd_util:to_list(http_proc_util:get_json_value(<<"account">>, DataList)) andalso
    AmountPPCoin =:= dd_util:to_list(http_proc_util:get_json_value(<<"amount">>, DataList)) andalso
    Status =:= dd_util:to_integer(http_proc_util:get_json_value(<<"status">>, DataList)) andalso
    AppId =:= dd_util:to_integer(http_proc_util:get_json_value(<<"app_id">>, DataList))
  catch
    What:Type ->
      ?FILE_LOG_ERROR("verify sign what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      false
  end.









