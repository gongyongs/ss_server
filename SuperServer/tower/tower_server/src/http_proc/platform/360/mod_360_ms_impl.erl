%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午3:11
%%%-------------------------------------------------------------------
-module(mod_360_ms_impl).
-author("zqlt").

-include("../../../dd_ms.hrl").
-include("../../http_proc.hrl").
-include("mod_360.hrl").
-include("../../../csv.hrl").
-include("../../../../deps/file_log/include/file_log.hrl").
%% API
-export([execute/3]).

execute(TableID, generate_orders, {BillNo, UserID, GoodID, Uin}) ->
  {success, CacheNode} = http_proc_util:get_cache_node(Uin),
  case rpc:call(CacheNode, cache, generate_order, [Uin, BillNo, GoodID, "360"]) of
    {success, GoodConfig} ->
      PriceVal = GoodConfig#res_goods.money_count,
      ets:insert(TableID, #rd_order_360{bill_no = BillNo, goods_id = GoodID, user_id = UserID, uin = Uin, time_stamp = dd_util:timestamp(), state = 1, order_price = PriceVal}),
      ?FILE_LOG_DEBUG("generate_360_order => id = ~p, good_id = ~p, user_id = ~p, uin = ~p", [BillNo, GoodID, UserID, Uin]),
      {success, {_, _, Path}} = http_proc_util:get_pay_cb_notify_by_mod(mod_360),
      {success, {BillNo, PriceVal, Path}};
    Other ->
      ?FILE_LOG_ERROR("generate_order fail ,reason  = ~p", [Other]),
      {fail, "SystemDataError"}
  end;

execute(TableID, complete_orders, {OrderID, BillNo, AccountID, AmountCoin, _ProductId, Flag, Rationality}) ->
  ?FILE_LOG_INFO("360 complete orders, [order_id][~p],[billno][~p],[userid][~p],[cost][~p], [rationality][~p]",[OrderID, BillNo, AccountID, AmountCoin, Rationality]),
  case ets:lookup(TableID, BillNo) of
    [] ->
      ?FILE_LOG_ERROR("Bill not exist, order = ~p, billno = ~p, account = ~p", [OrderID, BillNo, AccountID]),
      <<"ok">>;
    [BillInfo] ->
      try
        case Flag of
          "success" -> ok;
          _ -> throw({custom, 2, ?USER_PAY_FAILED})
        end,
        case Rationality of
          true -> ok;
          false ->
            ?FILE_LOG_ERROR("sign is not valid", []),
            throw({custom, 2, ?SIGNATURE_INVALID})
        end,
        case BillInfo#rd_order_360.state of
          0 ->
            ?FILE_LOG_ERROR("order has completed", []),
            throw({custom, 0, ?ORDER_COMPLETED});
          _ -> ok
        end,
        case check_price(AmountCoin, BillInfo#rd_order_360.order_price) of
          true -> ok;
          false ->
            ?FILE_LOG_ERROR("pay count not equal, price = ~p, pay = ~p", [BillInfo#rd_order_360.order_price, AmountCoin]),
            throw({custom, 2, ?GOOD_PRICE_INVALID})
        end,
        Uin = BillInfo#rd_order_360.uin,
        {success, CacheNode} = http_proc_util:get_cache_node(Uin),
        case rpc:call(CacheNode, cache, complete_orders, [Uin, "360", {OrderID, BillNo, AccountID, AmountCoin, BillInfo#rd_order_360.goods_id, BillInfo#rd_order_360.order_price}]) of
          {success, {_ID, UpdateInfo}} ->
            ?FILE_LOG_INFO("[~p][~p] complete order [~p] success, update info = ~p", [Uin, AccountID, OrderID, UpdateInfo]),
            NOrder = BillInfo#rd_order_360{state = 0, update_info = UpdateInfo},
            ets:insert(TableID, NOrder),
            http_proc_log:write_360_pay_log("CNPaySuccessFlow", {BillInfo#rd_order_360.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_360.goods_id, BillInfo#rd_order_360.order_price, AmountCoin, ?SUCCESS}),
            <<"ok">>;
          {fail, Reason} ->
            ?FILE_LOG_ERROR("[~p][~p] complete order [~p] error, reason = ~p", [Uin, AccountID, OrderID, Reason]),
            NRd3 = BillInfo#rd_order_360{state = 2},
            ets:insert(TableID, NRd3),
            http_proc_log:write_360_pay_log("CNPayFailFlow", {BillInfo#rd_order_360.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_360.goods_id, BillInfo#rd_order_360.order_price, AmountCoin, Reason}),
            <<"ok">>;
          Other ->
            ?FILE_LOG_ERROR("[~p][~p] complete order [~p] error, reason = ~p", [Uin, AccountID, OrderID, Other]),
            NRd2 = BillInfo#rd_order_360{state = 2},
            ets:insert(TableID, NRd2),
            http_proc_log:write_360_pay_log("CNPayFailFlow", {BillInfo#rd_order_360.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_360.goods_id, BillInfo#rd_order_360.order_price, AmountCoin, ?LOGICAL_ERROR}),
            <<"ok">>
        end
      catch
        throw:{custom, 0, _} ->
          <<"ok">>;
        throw:{custom, 2, Reason1} ->
          NPPRd1 = BillInfo#rd_order_360{state = 2},
          ets:insert(TableID, NPPRd1),
          http_proc_log:write_360_pay_log("CNPayFailFlow", {BillInfo#rd_order_360.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_360.goods_id, BillInfo#rd_order_360.order_price, AmountCoin, Reason1}),
          <<"ok">>;
        throw:{custom, Status, Reason2} ->
          NPPRd = BillInfo#rd_order_360{state = Status},
          ets:insert(TableID, NPPRd),
          http_proc_log:write_360_pay_log("CNPayFailFlow", {BillInfo#rd_order_360.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_360.goods_id, BillInfo#rd_order_360.order_price, AmountCoin, Reason2}),
          <<"ok">>;
        What:Type ->
          ?FILE_LOG_ERROR("complete order error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
          NRd1 = BillInfo#rd_order_360{state = 2},
          ets:insert(TableID, NRd1),
          http_proc_log:write_360_pay_log("CNPayFailFlow", {BillInfo#rd_order_360.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_360.goods_id, BillInfo#rd_order_360.order_price, AmountCoin, ?EXCEPTION}),
          <<"ok">>
      end
  end;
execute(TableID, query_order, BillNo) ->
  ?FILE_LOG_INFO("360 query order state, billno = ~p", [BillNo]),
  case ets:lookup(TableID, BillNo) of
    [] -> {fail, "HintBillNotExist"};
    [BillInfo] ->
      case BillInfo#rd_order_360.state of
        0 ->
          {success, BillInfo#rd_order_360.state, BillInfo#rd_order_360.update_info};
        2 -> {fail, "HintOrderFailed"};
        _ ->
          {success, BillInfo#rd_order_360.state}
      end
  end.

%%以分为单位
check_price(PayCount, Price) ->
  GoodsPrice = dd_util:float_to_integer(dd_util:list_to_float(Price, 0.0)*100), %%元转换为分
  Pay = dd_util:float_to_integer(dd_util:list_to_float(PayCount, 0.0) * 100),
  Pay =:= GoodsPrice.

