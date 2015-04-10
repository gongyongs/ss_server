%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 十一月 2014 下午1:20
%%%-------------------------------------------------------------------
-module(mod_pp_ms_impl).
-author("zqlt").

-include("../../../dd_ms.hrl").
-include("mod_pp.hrl").
-include("../../http_proc.hrl").
-include("../../../csv.hrl").
-include("../../../../deps/file_log/include/file_log.hrl").
%% API
-export([execute/3]).

execute(TableID, generate_orders, {BillNo, UserID, GoodID, Uin}) ->
  {success, CacheNode} = http_proc_util:get_cache_node(Uin),
  case rpc:call(CacheNode, cache, generate_order, [Uin, BillNo, GoodID, "PP"]) of
    {success, GoodConfig} ->
      PriceVal = GoodConfig#res_goods.money_count,
      ets:insert(TableID, #rd_order_pp{bill_no = BillNo, goods_id = GoodID, user_id = UserID, uin = Uin, time_stamp = dd_util:timestamp(), state = 1, order_price = PriceVal}),
      ?FILE_LOG_DEBUG("generate_pp_order => id = ~p, good_id = ~p, user_id = ~p, uin = ~p", [BillNo, GoodID, UserID, Uin]),
      {success, {_, _, Path}} = http_proc_util:get_pay_cb_notify_by_mod(mod_pp),
      {success, {BillNo, PriceVal, Path}};
    Other ->
      ?FILE_LOG_ERROR("generate_order fail ,reason  = ~p", [Other]),
      {fail, "SystemDataError"}
  end;

execute(TableID, complete_orders, {OrderID, BillNo, AccountID, AmountPPCoin, AppID, State, Rationality}) ->
  ?FILE_LOG_INFO("pp complete orders, [order_id][~p],[billno][~p],[userid][~p],[cost][~p], [rationality][~p]",[OrderID, BillNo, AccountID, AmountPPCoin, Rationality]),
  case ets:lookup(TableID, BillNo) of
    [] ->
      ?FILE_LOG_ERROR("Bill not exist, order = ~p, billno = ~p, account = ~p", [OrderID, BillNo, AccountID]),
      <<"success">>;
    [BillInfo] ->
      try
        if
          AppID =:= ?APP_ID_PP -> ok;
          true ->
            ?FILE_LOG_ERROR("complete order=> app id not match , real = ~p, notify = ~p", [?APP_ID_PP, AppID]),
            throw({custom, 2, ?APP_ID_INVALID})
        end,
        case State of
          0 -> ok;
          1 -> throw({custom, 0, ?ORDER_COMPLETED})
        end,
        case Rationality of
          true -> ok;
          false ->
            ?FILE_LOG_ERROR("sign is not valid", []),
            throw({custom, 2, ?SIGNATURE_INVALID})
        end,
        case check_price(AmountPPCoin, BillInfo#rd_order_pp.order_price) of
          true -> ok;
          false ->
            ?FILE_LOG_ERROR("pay count not equal, price = ~p, pay = ~p", [BillInfo#rd_order_pp.order_price, AmountPPCoin]),
            throw({custom, 2, ?GOOD_PRICE_INVALID})
        end,
        case BillInfo#rd_order_pp.state of
          0 ->
            ?FILE_LOG_ERROR("order has completed", []),
            throw({custom, 0, ?ORDER_COMPLETED});
          _ -> ok
        end,
        Uin = BillInfo#rd_order_pp.uin,
        {success, CacheNode} = http_proc_util:get_cache_node(Uin),
        case rpc:call(CacheNode, cache, complete_orders, [Uin, "PP", {OrderID, BillNo, AccountID, AmountPPCoin, BillInfo#rd_order_pp.goods_id, BillInfo#rd_order_pp.order_price}]) of
          {success, {_ID, UpdateInfo}} ->
            ?FILE_LOG_INFO("[~p][~p] complete order [~p] success, update info = ~p", [Uin, AccountID, OrderID, UpdateInfo]),
            NOrder = BillInfo#rd_order_pp{state = 0, update_info = UpdateInfo},
            ets:insert(TableID, NOrder),
            http_proc_log:write_pp_pay_log("CNPaySuccessFlow", {BillInfo#rd_order_pp.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_pp.goods_id, BillInfo#rd_order_pp.order_price, AmountPPCoin, ?SUCCESS}),
            <<"success">>;
          {fail, Reason} ->
            ?FILE_LOG_ERROR("[~p][~p] complete order [~p] error, reason = ~p", [Uin, AccountID, OrderID, Reason]),
            NRd3 = BillInfo#rd_order_pp{state = 2},
            ets:insert(TableID, NRd3),
            http_proc_log:write_pp_pay_log("CNPayFailFlow", {BillInfo#rd_order_pp.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_pp.goods_id, BillInfo#rd_order_pp.order_price, AmountPPCoin, Reason}),
            <<"success">>;
          Other ->
            ?FILE_LOG_ERROR("[~p][~p] complete order [~p] error, reason = ~p", [Uin, AccountID, OrderID, Other]),
            NRD = BillInfo#rd_order_pp{state = 2},
            ets:insert(TableID, NRD),
            http_proc_log:write_pp_pay_log("CNPayFailFlow", {BillInfo#rd_order_pp.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_pp.goods_id, BillInfo#rd_order_pp.order_price, AmountPPCoin, ?LOGICAL_ERROR}),
            <<"success">>
        end
      catch
        throw:{custom, 0, _} ->
          <<"success">>;
        throw:{custom, Status, Reason1} ->
          NPPRd = BillInfo#rd_order_pp{state = Status},
          ets:insert(TableID, NPPRd),
          http_proc_log:write_pp_pay_log("CNPayFailFlow", {BillInfo#rd_order_pp.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_pp.goods_id, BillInfo#rd_order_pp.order_price, AmountPPCoin, Reason1}),
          <<"success">>;
        What:Type ->
          ?FILE_LOG_ERROR("complete order error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
          NRd = BillInfo#rd_order_pp{state = 2},
          ets:insert(TableID, NRd),
          http_proc_log:write_pp_pay_log("CNPayFailFlow", {BillInfo#rd_order_pp.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_pp.goods_id, BillInfo#rd_order_pp.order_price, AmountPPCoin, ?EXCEPTION}),
          <<"success">>
      end
  end;
execute(TableID, query_order, BillNo) ->
  ?FILE_LOG_INFO("pp query order state, billno = ~p", [BillNo]),
  case ets:lookup(TableID, BillNo) of
    [] -> {fail, "HintBillNotExist"};
    [BillInfo] ->
      case BillInfo#rd_order_pp.state of
        0 ->
          {success, BillInfo#rd_order_pp.state, BillInfo#rd_order_pp.update_info};
        2 -> {fail, "HintOrderFailed"};
        _ ->
          {success, BillInfo#rd_order_pp.state}
      end
  end.

%%以分为单位
check_price(PayCount, Price) ->
  GoodsPrice = dd_util:float_to_integer(dd_util:list_to_float(Price, 0.0)*100), %%元转换为分
  Pay = dd_util:float_to_integer(dd_util:list_to_float(PayCount, 0.0) * 100),
  Pay =:= GoodsPrice.


