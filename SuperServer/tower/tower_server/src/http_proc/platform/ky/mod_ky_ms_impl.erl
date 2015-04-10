%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 一月 2015 下午5:28
%%%-------------------------------------------------------------------
-module(mod_ky_ms_impl).
-author("zqlt").

-include("../../../dd_ms.hrl").
-include("../../http_proc.hrl").
-include("mod_ky.hrl").
-include("../../../csv.hrl").
-include("../../../../deps/file_log/include/file_log.hrl").
%% API
-export([execute/3]).

execute(TableID, generate_orders, {BillNo, UserID, GoodID, Uin}) ->
  {success, CacheNode} = http_proc_util:get_cache_node(Uin),
  case rpc:call(CacheNode, cache, generate_order, [Uin, BillNo, GoodID, "KY"]) of
    {success, GoodConfig} ->
      PriceVal = GoodConfig#res_goods.money_count,
      ets:insert(TableID, #rd_order_ky{bill_no = BillNo, goods_id = GoodID, user_id = UserID, uin = Uin, time_stamp = dd_util:timestamp(), state = 1, order_price = PriceVal}),
      ?FILE_LOG_DEBUG("generate_KY_order => id = ~p, good_id = ~p, user_id = ~p, uin = ~p", [BillNo, GoodID, UserID, Uin]),
      {success, {_, _, Path}} = http_proc_util:get_pay_cb_notify_by_mod(mod_ky),
      {success, {BillNo, PriceVal, Path}};
    Other ->
      ?FILE_LOG_ERROR("generate_order fail ,reason  = ~p", [Other]),
      {fail, "SystemDataError"}
  end;

execute(TableID, complete_orders, {OrderID, BillNo, AccountID, Fee, _ProductId, PayResult, Rationality}) ->
  ?FILE_LOG_INFO("ky complete orders, [order_id][~p],[billno][~p],[userid][~p],[cost][~p], [rationality][~p]",[OrderID, BillNo, AccountID, Fee, Rationality]),
  case ets:lookup(TableID, BillNo) of
    [] ->
      ?FILE_LOG_ERROR("Bill not exist, order = ~p, billno = ~p, account = ~p", [OrderID, BillNo, AccountID]),
      <<"success">>;
    [BillInfo] ->
      try
        case BillInfo#rd_order_ky.state of
          0 ->
            ?FILE_LOG_ERROR("order has completed, billno = ~p, orderid = ~p", [BillNo, OrderID]),
            throw({custom, 0, ?ORDER_COMPLETED});
          _ -> ok
        end,
        %%校验支付结果
        case PayResult of
          0 -> ok;
          -1 ->
            ?FILE_LOG_ERROR("ky pay fail, billno = ~p, orderid = ~p", [BillNo, OrderID]),
            throw({custom, 2, ?USER_PAY_FAILED});
          -2 ->
            ?FILE_LOG_ERROR("ky pay timeout, billno = ~p, orderid = ~p", [BillNo, OrderID]),
            throw({custom, 2, ?USER_PAY_TIMEOUT})
        end,
        %%校验签名
        case Rationality of
          true -> ok;
          false ->
            ?FILE_LOG_ERROR("sign is not valid, billno = ~p, orderid = ~p", [BillNo, OrderID]),
            throw({custom, 2, ?SIGNATURE_INVALID})
        end,
        Uin = BillInfo#rd_order_ky.uin,
        {success, CacheNode} = http_proc_util:get_cache_node(Uin),
        case rpc:call(CacheNode, cache, complete_orders, [Uin, "KY", {OrderID, BillNo, AccountID, Fee, BillInfo#rd_order_ky.goods_id, BillInfo#rd_order_ky.order_price}]) of
          {success, {_ID, UpdateInfo}} ->
            ?FILE_LOG_INFO("[~p][~p] complete order [~p] success, update info = ~p", [Uin, AccountID, OrderID, UpdateInfo]),
            NOrder = BillInfo#rd_order_ky{state = 0, update_info = UpdateInfo},
            ets:insert(TableID, NOrder),
            http_proc_log:write_ky_pay_log("CNPaySuccessFlow", {BillInfo#rd_order_ky.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_ky.goods_id, BillInfo#rd_order_ky.order_price, Fee, ?SUCCESS}),
            <<"success">>;
          {fail, Reason} ->
            ?FILE_LOG_ERROR("[~p][~p] complete order [~p] error, reason = ~p", [Uin, AccountID, OrderID, Reason]),
            NRd3 = BillInfo#rd_order_ky{state = 2},
            ets:insert(TableID, NRd3),
            http_proc_log:write_ky_pay_log("CNPayFailFlow", {BillInfo#rd_order_ky.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_ky.goods_id, BillInfo#rd_order_ky.order_price, Fee, Reason}),
            <<"success">>;
          Other ->
            ?FILE_LOG_ERROR("[~p][~p] complete order [~p] error, reason = ~p", [Uin, AccountID, OrderID, Other]),
            NRd2 = BillInfo#rd_order_ky{state = 2},
            ets:insert(TableID, NRd2),
            http_proc_log:write_ky_pay_log("CNPayFailFlow", {BillInfo#rd_order_ky.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_ky.goods_id, BillInfo#rd_order_ky.order_price, Fee, ?LOGICAL_ERROR}),
            <<"success">>
        end
      catch
        throw:{custom, 0, _} ->
          <<"success">>;
        throw:{custom, Status, Reason1} ->
          NPPRd = BillInfo#rd_order_ky{state = Status},
          ets:insert(TableID, NPPRd),
          http_proc_log:write_ky_pay_log("CNPayFailFlow", {BillInfo#rd_order_ky.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_ky.goods_id, BillInfo#rd_order_ky.order_price, Fee, Reason1}),
          <<"success">>;
        What:Type ->
          ?FILE_LOG_ERROR("complete order error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
          NRd1 = BillInfo#rd_order_ky{state = 2},
          ets:insert(TableID, NRd1),
          http_proc_log:write_ky_pay_log("CNPayFailFlow", {BillInfo#rd_order_ky.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_ky.goods_id, BillInfo#rd_order_ky.order_price, Fee, ?EXCEPTION}),
          <<"success">>
      end
  end;
execute(TableID, query_order, BillNo) ->
  ?FILE_LOG_INFO("ky query order state, billno = ~p", [BillNo]),
  case ets:lookup(TableID, BillNo) of
    [] -> {fail, "HintBillNotExist"};
    [BillInfo] ->
      case BillInfo#rd_order_ky.state of
        0 ->
          {success, BillInfo#rd_order_ky.state, BillInfo#rd_order_ky.update_info};
        2 -> {fail, "HintOrderFailed"};
        _ ->
          {success, BillInfo#rd_order_ky.state}
      end
  end.
