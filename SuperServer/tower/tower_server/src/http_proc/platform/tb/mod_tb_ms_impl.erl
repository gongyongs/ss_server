%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2015 上午10:58
%%%-------------------------------------------------------------------
-module(mod_tb_ms_impl).
-author("zqlt").


-include("../../../dd_ms.hrl").
-include("mod_tb.hrl").
-include("../../http_proc.hrl").
-include("../../../csv.hrl").
-include("../../../../deps/file_log/include/file_log.hrl").
%% API
-export([execute/3]).

success() ->
  dd_util:encode_json_utf8({struct, [{<<"status">>, <<"suuccess">>}]}).

%% fail() ->
%%   dd_util:encode_json_utf8({struct, [{<<"status">>, <<"fail">>}]}).

execute(TableID, generate_orders, {BillNo, UserID, GoodID, Uin}) ->
  {success, CacheNode} = http_proc_util:get_cache_node(Uin),
  case rpc:call(CacheNode, cache, generate_order, [Uin, BillNo, GoodID, "TB"]) of
    {success, GoodConfig} ->
      PriceVal = GoodConfig#res_goods.money_count,
      ets:insert(TableID, #rd_order_tb{bill_no = BillNo, goods_id = GoodID, user_id = UserID, uin = Uin, time_stamp = dd_util:timestamp(), state = 1, order_price = PriceVal}),
      ?FILE_LOG_DEBUG("generate_tb_order => id = ~p, good_id = ~p, user_id = ~p, uin = ~p", [BillNo, GoodID, UserID, Uin]),
      {success, {_, _, Path}} = http_proc_util:get_pay_cb_notify_by_mod(mod_tb),
      {success, {BillNo, PriceVal, Path}};
    Other ->
      ?FILE_LOG_ERROR("generate_order fail ,reason  = ~p", [Other]),
      {fail, "SystemDataError"}
  end;

execute(TableID, complete_orders, {OrderID, BillNo, AccountID, AmountCoin, AppID, DebugFlag, Rationality}) ->
  ?FILE_LOG_INFO("tb complete orders, [order_id][~p],[billno][~p],[userid][~p],[cost][~p], [rationality][~p]",[OrderID, BillNo, AccountID, AmountCoin, Rationality]),
  case ets:lookup(TableID, BillNo) of
    [] ->
      ?FILE_LOG_ERROR("Bill not exist, order = ~p, billno = ~p, account = ~p", [OrderID, BillNo, AccountID]),
      success();
    [BillInfo] ->
      try
        if
          AppID =:= ?APP_ID_TB -> ok;
          true ->
            ?FILE_LOG_ERROR("app id not equal", []),
            throw({custom, 2, ?APP_ID_INVALID})
        end,
        case DebugFlag of     %%调试模式开启和关闭
          1 -> ok;
            %%throw({custom, 2, ?PAY_DEBUG_MODE});
          0 -> ok
        end,
        case Rationality of
          true -> ok;
          false ->
            ?FILE_LOG_ERROR("sign is not valid", []),
            throw({custom, 2,  ?SIGNATURE_INVALID})
        end,
        case BillInfo#rd_order_tb.state of
          0 ->
            ?FILE_LOG_ERROR("order has completed", []),
            throw({custom, 0, ?ORDER_COMPLETED});
          _ -> ok
        end,
        Uin = BillInfo#rd_order_tb.uin,
        {success, CacheNode} = http_proc_util:get_cache_node(Uin),
        case rpc:call(CacheNode, cache, complete_orders, [Uin, "TB", {OrderID, BillNo, AccountID, AmountCoin, BillInfo#rd_order_tb.goods_id, BillInfo#rd_order_tb.order_price}]) of
          {success, {_ID, UpdateInfo}} ->
            ?FILE_LOG_INFO("[~p][~p] complete order [~p] success, update info = ~p", [Uin, AccountID, OrderID, UpdateInfo]),
            NOrder = BillInfo#rd_order_tb{state = 0, update_info = UpdateInfo},
            ets:insert(TableID, NOrder),
            http_proc_log:write_tb_pay_log("CNPaySuccessFlow", {BillInfo#rd_order_tb.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_tb.goods_id, BillInfo#rd_order_tb.order_price, AmountCoin, ?SUCCESS}),
            success();
          {fail, Reason} ->
            ?FILE_LOG_ERROR("[~p][~p] complete order [~p] error, reason = ~p", [Uin, AccountID, OrderID, Reason]),
            NRd3 = BillInfo#rd_order_tb{state = 2},
            ets:insert(TableID, NRd3),
            http_proc_log:write_tb_pay_log("CNPayFailFlow", {BillInfo#rd_order_tb.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_tb.goods_id, BillInfo#rd_order_tb.order_price, AmountCoin, Reason}),
            success();
          Other ->
            ?FILE_LOG_ERROR("[~p][~p] complete order [~p] error, reason = ~p", [Uin, AccountID, OrderID, Other]),
            NRd2 = BillInfo#rd_order_tb{state = 2},
            ets:insert(TableID, NRd2),
            http_proc_log:write_tb_pay_log("CNPayFailFlow", {BillInfo#rd_order_tb.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_tb.goods_id, BillInfo#rd_order_tb.order_price, AmountCoin, ?LOGICAL_ERROR}),
            success()
        end
      catch
        throw:{custom, 0, _} -> success();
        throw:{custom, Status, Reason1} ->
          NPPRd = BillInfo#rd_order_tb{state = Status},
          ets:insert(TableID, NPPRd),
          http_proc_log:write_tb_pay_log("CNPayFailFlow", {BillInfo#rd_order_tb.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_tb.goods_id, BillInfo#rd_order_tb.order_price, AmountCoin, Reason1}),
          success();
        What:Type ->
          ?FILE_LOG_ERROR("complete order error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
          NRd1 = BillInfo#rd_order_tb{state = 2},
          ets:insert(TableID, NRd1),
          http_proc_log:write_tb_pay_log("CNPayFailFlow", {BillInfo#rd_order_tb.uin, OrderID, BillNo, AccountID, BillInfo#rd_order_tb.goods_id, BillInfo#rd_order_tb.order_price, AmountCoin, ?EXCEPTION}),
          success()
      end
  end;
execute(TableID, query_order, BillNo) ->
  ?FILE_LOG_INFO("tb query order state, billno = ~p", [BillNo]),
  case ets:lookup(TableID, BillNo) of
    [] -> {fail, "HintBillNotExist"};
    [BillInfo] ->
      case BillInfo#rd_order_tb.state of
        0 ->
          {success, BillInfo#rd_order_tb.state, BillInfo#rd_order_tb.update_info};
        2 -> {fail, "HintOrderFailed"};
        _ ->
          {success, BillInfo#rd_order_tb.state}
      end
  end.
