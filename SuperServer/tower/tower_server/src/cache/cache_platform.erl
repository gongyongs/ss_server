%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 一月 2015 下午3:07
%%%-------------------------------------------------------------------
-module(cache_platform).
-author("zqlt").
-include("../csv.hrl").
-include("cache_def.hrl").
-include("../../deps/file_log/include/file_log.hrl").

%% API
-export([
  execute/1
]).
%%货币已全部转换为字符串类型的，且单位是元
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%PP助手平台%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute({generate_order, "PP", {Uin, BillNo, GoodsID}}) ->
  generate_plat_order("PP", {Uin, BillNo, GoodsID});

execute({complete_orders, {Uin, "PP", {OrderID, BillNo, AccountID, AmountCoin, GoodsID, Price}}}) ->
  complete_plat_order("PP", {Uin, {OrderID, BillNo, AccountID, AmountCoin, GoodsID, Price}}, false);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%360平台
execute({generate_order, "360", {Uin, BillNo, GoodsID}}) ->
  generate_plat_order("360", {Uin, BillNo, GoodsID});

execute({complete_orders, {Uin, "360", {OrderID, BillNo, AccountID, AmountCoin, GoodsID, Price}}}) ->
  complete_plat_order("360", {Uin, {OrderID, BillNo, AccountID, AmountCoin, GoodsID, Price}}, false);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%快用平台
execute({generate_order, "KY", {Uin, BillNo, GoodsID}}) ->
  generate_plat_order("KY", {Uin, BillNo, GoodsID});

execute({complete_orders, {Uin, "KY", {OrderID, BillNo, AccountID, AmountCoin, GoodsID, Price}}}) ->
  complete_plat_order("KY", {Uin, {OrderID, BillNo, AccountID, AmountCoin, GoodsID, Price}}, true);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%同步推
execute({generate_order, "TB", {Uin, BillNo, GoodsID}}) ->
  generate_plat_order("TB", {Uin, BillNo, GoodsID});

execute({complete_orders, {Uin, "TB", {OrderID, BillNo, AccountID, AmountCoin, GoodsID, Price}}}) ->
  complete_plat_order("TB", {Uin, {OrderID, BillNo, AccountID, AmountCoin, GoodsID, Price}}, false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_plat_order(Plat, {Uin, BillNo, GoodsID}) ->
  ?FILE_LOG_DEBUG("generate_plat_order => plat = ~p, uin = ~p, billno = ~p, goodsid = ~p", [Plat, Uin, BillNo, GoodsID]),
  not_exist = cache_work_proc:get_charge_order(Uin, dd_util:to_list(BillNo)),
  CommodityConfig = cache_configure_data:get_commodity_config(GoodsID),
  success = cache_work_proc:add_charge_order(Uin, dd_util:to_list(BillNo)),
  ?FILE_LOG_DEBUG("generate order success", []),
  {success, CommodityConfig}.

complete_plat_order(Plat, {Uin, {OrderID, BillNo, AccountID, AmountCoin, GoodsID, Price}}, NeedOffset) ->
  {success, OAccount} = cache_work_proc:get_account(Uin),
  {Account, TFieldList} = cache_work_proc:update_account_to_time(OAccount),

  ?FILE_LOG_DEBUG("complete plat order => plat = ~p, uin = ~p, orderid = ~p, billno = ~p, accountid = ~p, amountcoin = ~p, goodsid = ~p, price = ~p",
    [Plat, Uin, OrderID, BillNo, AccountID, AmountCoin, GoodsID, Price]),

  {{NAccount, FieldList, {BuyCount}}, Commodity} =
    try
      %%检查钻石id是否存在
      CommodityConfig = cache_configure_data:get_commodity_config(GoodsID),
      check_order_valid(Account, BillNo, CommodityConfig),
      {complete_orders(Account, BillNo, dd_util:to_list(AmountCoin), CommodityConfig, NeedOffset), CommodityConfig}
    catch
      throw:{custom, Reason} ->
        OrderInfo = AccountID ++ "|" ++ dd_util:to_list(OrderID) ++ "|" ++ BillNo ++ "|" ++ dd_util:to_list(AmountCoin) ++ "|" ++ GoodsID,
        cache_log_util:write_fail_pay_log(Account#account.uin, Plat, dd_util:to_list(OrderID), OrderInfo, Reason),
        ?FILE_LOG_DEBUG("complete_plat_order error, [~p]", [Reason]),
        throw({custom, "HintPurchaseError"});
      What1:Type1 ->
        OrderInfo1 = AccountID ++ "|" ++ BillNo ++ "|" ++ dd_util:to_list(AmountCoin) ++ "|" ++ GoodsID,
        cache_log_util:write_fail_pay_log(Account#account.uin, Plat, dd_util:to_list(OrderID), OrderInfo1, "HandleException"),
        ?FILE_LOG_ERROR("complete_plat_order error , What = ~p, type = ~p, stack = ~p", [What1, Type1, erlang:get_stacktrace()]),
        throw({fail, "HintPurchaseError"})
    end,
  %%更新成就和任务
  {UpdateMissionList, Mission} = cache_mission:update_mission(charge, Uin, NAccount#account.mission, BuyCount),
  NNAccount = NAccount#account{mission = Mission},
  NFieldList = lists:umerge3(TFieldList,FieldList, ["mission"]),
  ?FILE_LOG_DEBUG("complete_plat_order success", []),
  success = cache_work_proc:update_account(NNAccount, NFieldList),

  OrderInfo2 = AccountID ++ "|" ++ dd_util:to_list(OrderID) ++ "|" ++ BillNo ++ "|" ++ dd_util:to_list(AmountCoin) ++ "|" ++ GoodsID,
  %%输出流水日志
  cache_log_util:write_success_pay_log(Account#account.uin, Plat, dd_util:to_list(OrderID), GoodsID, Commodity#res_goods.goods_type, [], Commodity#res_goods.goods_count, dd_util:to_list(OrderInfo2), Commodity#res_goods.money_count),

  {success, {OrderID, cache_work_util:encode_platform_pay(NNAccount, UpdateMissionList)}}.


check_order_valid(Account, BillNo, CommodityConfig) when is_record(Account, account) andalso is_list(BillNo) andalso is_record(CommodityConfig, res_goods) ->
  {success, _} = cache_work_proc:get_charge_order(Account#account.uin, dd_util:to_list(BillNo)),
  case CommodityConfig#res_goods.goods_type of
    1 -> %%钻石
      RestrictCount = CommodityConfig#res_goods.restrict_count,
      if
        RestrictCount > 0 -> %%检查 限购次数
          case cache_util:find_commodity_by_id(Account#account.shop#shop.goods_list, CommodityConfig#res_goods.id) of
            {success, GoodsItem} ->
              TRemainCount = GoodsItem#goods.remain_count,
              if
                TRemainCount > 0 -> ok;
                true ->
                  ?FILE_LOG_WARNING("buy goods: goods [~p] has no chance!", [CommodityConfig#res_goods.id]),
                  throw({custom, "HintNoPuchaseChance"})
              end;
            fail ->
              ?FILE_LOG_WARNING("commodity data error, [~p]", [CommodityConfig#res_goods.id]),
              ok
          end;
        true -> ok
      end,
      %%检查是否在销售期内
      CurTime = dd_util:timestamp(),
      StartTs =  CommodityConfig#res_goods.start_ts,
      OverTs =  CommodityConfig#res_goods.over_ts,
      if
        StartTs =< 0 -> ok; %%没有限购期
        StartTs > 0 andalso CurTime >= StartTs andalso CurTime =< OverTs -> ok;
        true ->
          ?FILE_LOG_WARNING("commodity not in the sales period.", []),
          throw({custom, "HintNotAllowPurchase"})
      end;
    Other ->
      ?FILE_LOG_DEBUG("purchase type error", [Other]),
      throw({custom, "HintPurchaseError"})
  end.

%%NeedOffset表示是否需要根据实际支付的钱与真实支付的钱做对比，并进行相关的补偿
%%一块钱十个钻： 1 = 10
complete_orders(Account, _BillNo, AmountCoin, CommodityConfig, NeedOffset) when is_record(Account, account) andalso is_list(AmountCoin) andalso is_record(CommodityConfig, res_goods) ->
  %%
  GoodsPrice = dd_util:float_to_integer(dd_util:list_to_float(CommodityConfig#res_goods.money_count, 0.0)*100), %%元转换为分
  PayCount = dd_util:float_to_integer(dd_util:list_to_float(AmountCoin, 0.0) * 100),
  ?FILE_LOG_DEBUG("complete order:  goods price = ~p, paycount = ~p", [GoodsPrice, PayCount]),
  %%更新售卖次数
  RestrictCount = CommodityConfig#res_goods.restrict_count,
  GoodsList =
    if
      RestrictCount > 0 ->
        case cache_util:find_commodity_by_id(Account#account.shop#shop.goods_list, CommodityConfig#res_goods.id) of
          fail -> [#goods{id = CommodityConfig#res_goods.id, remain_count = CommodityConfig#res_goods.restrict_count - 1, latest_buy_ts = dd_util:timestamp()} | Account#account.shop#shop.goods_list];
          {success, Item} -> cache_util:update_commodity(Account#account.shop#shop.goods_list, Item#goods{remain_count = Item#goods.remain_count - 1, latest_buy_ts = dd_util:timestamp()})
        end;
      true -> Account#account.shop#shop.goods_list
    end,

  %%核对价钱
  Account3 =  complete_order_add_gem(NeedOffset, Account, PayCount, GoodsPrice, CommodityConfig),

  PayPrice = dd_util:list_to_float(AmountCoin, 0.0),
  PayItem = #pay_item{pay_ts = dd_util:timestamp(), pay_item_id = CommodityConfig#res_goods.id, pay_gem_count = CommodityConfig#res_goods.goods_count, pay_price = PayPrice},
  TotalPayVal = Account3#account.shop#shop.pay_info#pay_info.total_pay_val + PayPrice,
  TotalGemCount = Account3#account.shop#shop.pay_info#pay_info.total_gem_count + CommodityConfig#res_goods.goods_count,
  NRecord = [PayItem | Account3#account.shop#shop.pay_info#pay_info.pay_record],
  NPayInfo =
    case Account3#account.shop#shop.pay_info#pay_info.first_pay_ts of
      0 ->
        Account3#account.shop#shop.pay_info#pay_info{first_pay_ts = dd_util:timestamp(), pay_record = NRecord, total_pay_val = TotalPayVal, total_gem_count = TotalGemCount};
      _ ->
        Account3#account.shop#shop.pay_info#pay_info{pay_record = NRecord, total_pay_val = TotalPayVal, total_gem_count = TotalGemCount}
    end,

  Shop = Account3#account.shop#shop{goods_list = GoodsList, pay_info = NPayInfo},
  {Account3#account{shop = Shop}, ["shop", "gold_coin", "gem"], {CommodityConfig#res_goods.goods_count}}.


complete_order_add_gem(NeedOffset, Account, PayCount, GoodsPrice, CommodityConfig) ->
  {NAccount, NeedGift} =
    case NeedOffset of
      true ->
        if
          PayCount < GoodsPrice ->
            ?FILE_LOG_DEBUG("user pay count is not enough, 1:10 give gem!pay = ~p, price = ~p", [PayCount, GoodsPrice]),
            BuyGem = PayCount div 10,
            {cache_api:account_inc_money(Account, 1, BuyGem, "Charge"), false};
          PayCount > GoodsPrice ->
            ?FILE_LOG_DEBUG("user pay count is too enough, 1:10 give gem!pay = ~p, price = ~p", [PayCount, GoodsPrice]),
            %%更新金币钻石或者道具
            GoodsCount = CommodityConfig#res_goods.goods_count,
            Compensate = (PayCount - GoodsPrice) div 10,
            ?FILE_LOG_DEBUG("turely buy gem is ~p", [Compensate + GoodsCount]),
            {cache_api:account_inc_money(Account, 1, GoodsCount + Compensate, "Charge"), true};
          true ->
            %%更新金币钻石或者道具
            BuyCount = CommodityConfig#res_goods.goods_count,
            {cache_api:account_inc_money(Account, 1, BuyCount, "Charge"), true}
        end;
      false ->
        %%更新金币钻石或者道具
        BuyCount = CommodityConfig#res_goods.goods_count,
        {cache_api:account_inc_money(Account, 1, BuyCount, "Charge"), true}
    end,
  %%增加赠送
  case NeedGift of
    false -> NAccount;
    true ->
      case CommodityConfig#res_goods.gift_type of
        1 -> %%赠送钻石
          cache_api:account_inc_money(NAccount, 1, CommodityConfig#res_goods.gift_count, "BuyGemGift");
        2 -> %%赠送金币
          cache_api:account_inc_money(NAccount, 2, CommodityConfig#res_goods.gift_count, "BuyGemGift");
        OtherType ->
          ?FILE_LOG_INFO("buy goods gift other type [~p]", [OtherType]),
          NAccount
      end
  end.
