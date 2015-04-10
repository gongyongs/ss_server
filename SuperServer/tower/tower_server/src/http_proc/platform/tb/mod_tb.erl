%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2015 上午10:57
%%%-------------------------------------------------------------------
-module(mod_tb).
-author("zqlt").
-include("mod_tb.hrl").
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
  mod_tb_ms:query_order(UserID, BillNo).

generate_orders(UserID, GoodsID, Uin) when is_list(UserID) andalso is_list(GoodsID) andalso is_integer(Uin) ->
  ?FILE_LOG_DEBUG("generate_order => userid = ~p, goodid = ~p, uin = ~p", [UserID, GoodsID, Uin]),
  mod_tb_ms:generate_orders(UserID, GoodsID, Uin).

call_back_handle(Req) ->
  PostData = Req:parse_qs(),
  ?FILE_LOG_DEBUG("TB POST = ~p", [PostData]),
  try
    Source = dd_util:to_list(proplists:get_value("source", PostData, undefined)),
    BillNo = dd_util:to_list(proplists:get_value("trade_no", PostData, undefined)),    %%订单号
    Amount = dd_util:to_integer(proplists:get_value("amount", PostData, undefined)),    %%支付多少钱，以分为单位(单位：分)
    AppID = dd_util:to_integer(proplists:get_value("partner", PostData, undefined)),    %%appid
    AccountID = dd_util:to_list(proplists:get_value("paydes", PostData, undefined)),      %%支付说明(玩家平台id)
    Debug = dd_util:to_integer(proplists:get_value("debug", PostData, undefined)),      %%调试模式
    OrderID = dd_util:to_list(proplists:get_value("tborder", PostData, undefined)),    %%订单编号
    Sign = dd_util:to_list(proplists:get_value("sign", PostData, undefined)),           %%签名
    %%SIGN
    TmpSign = "source=" ++ Source ++ "&trade_no=" ++ BillNo ++ "&amount=" ++ dd_util:to_list(Amount) ++ "&partner=" ++ dd_util:to_list(AppID) ++
      "&paydes=" ++ AccountID ++ "&debug=" ++ dd_util:to_list(Debug) ++ "&tborder=" ++ OrderID ++ "&key=" ++ ?APP_KEY_TB,
    Md5Sign = dd_util:md5_string(TmpSign),
    SignResult = (Sign =:= Md5Sign),
    TrueAmount = dd_util:float_to_string(Amount / 100.0),  %%全部改成以元为单位
    mod_tb_ms:complete_orders(OrderID, BillNo, AccountID, TrueAmount, AppID, Debug, SignResult)
  catch
    What:Type ->
      ?FILE_LOG_WARNING("call_back_handle error what=~p, type=~p stack=~p.", [What, Type, erlang:get_stacktrace()]),
      <<"ok">>
  end.
