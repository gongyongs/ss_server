%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午3:10
%%%-------------------------------------------------------------------
-module(mod_360).
-author("zqlt").
-include("mod_360.hrl").
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
  mod_360_ms:query_order(UserID, BillNo).

generate_orders(UserID, GoodsID, Uin) when is_list(UserID) andalso is_list(GoodsID) andalso is_integer(Uin) ->
  ?FILE_LOG_DEBUG("generate_order => userid = ~p, goodid = ~p, uin = ~p", [UserID, GoodsID, Uin]),
  mod_360_ms:generate_orders(UserID, GoodsID, Uin).

call_back_handle(Req) ->
  PostData = Req:parse_qs(),
  ?FILE_LOG_DEBUG("360 POST = ~p", [PostData]),
  try
    Sign = dd_util:to_list(proplists:get_value("sign", PostData, undefined)),
    AccountID = dd_util:to_list(proplists:get_value("user_id", PostData, undefined)),        	%%360用户id
    ProductId = dd_util:to_list(proplists:get_value("product_id", PostData, undefined)),  	%%商品id
    Amount = dd_util:to_integer(proplists:get_value("amount", PostData, undefined)),      	%%消耗的钱(分)
    OrderID = dd_util:to_integer(proplists:get_value("order_id", PostData, undefined)),     %%360的订单号
    AppOrderId = dd_util:to_list(proplists:get_value("app_order_id", PostData, undefined)), %%订单id
    AppKey = dd_util:to_list(proplists:get_value("app_key", PostData, undefined)),
    Flag = dd_util:to_list(proplists:get_value("gateway_flag", PostData, undefined)),
    true = (AppKey =:= ?APP_KEY_360),
    %%删除sign和sign_return
    P1 = proplists:delete("sign", PostData),
    P2 = proplists:delete("sign_return", P1),
    P3 = clean_empty_value(P2, []),
    P4 = http_proc_util:eksort(P3),
    TmpSign = acc_value(P4, "") ++ "#" ++ ?APP_SECRET_360,
    Md5Sign = dd_util:md5_string(TmpSign),
    SignResult = (Sign =:= Md5Sign),
    TrueAmount = dd_util:float_to_string(Amount / 100.0),
    mod_360_ms:complete_orders(OrderID, AppOrderId, AccountID, TrueAmount, ProductId, Flag, SignResult)
  catch
    What:Type ->
      ?FILE_LOG_WARNING("call_back_handle error what=~p, type=~p stack=~p.", [What, Type, erlang:get_stacktrace()]),
      <<"ok">>
  end.


clean_empty_value([], R) -> R;
clean_empty_value([{_, ""}|T], R) -> clean_empty_value(T, R);
clean_empty_value([V|T], R) -> clean_empty_value(T, [V|R]).

acc_value([], R) -> R;
acc_value([{_, V}|T], "") -> acc_value(T, V);
acc_value([{_,V}|T], R) -> acc_value(T, R ++ "#" ++ V).




