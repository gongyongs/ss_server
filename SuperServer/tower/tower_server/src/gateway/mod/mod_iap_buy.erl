%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 九月 2014 下午3:56
%%%-------------------------------------------------------------------
-module(mod_iap_buy).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
%% API
-export([req_handle/1]).

write_fail_log(CacheNode, Uin, Receipt, Reason) ->
  case rpc:call(CacheNode, cache_log, log_fail_pay_flow, [Uin, "IOS", "", dd_util:timestamp(), Receipt, Reason]) of
    ok ->
      ?FILE_LOG_DEBUG("write fail pay success", []);
    Other ->
      ?FILE_LOG_DEBUG("write fail pay fail, uin = ~p, receipt= ~p, reason = ~p, error = ~p",[Uin, Receipt, Reason, Other])
  end.


req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  PostData = Req:parse_post(),
  SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  Receipt = dd_util:to_binary(gateway_util:undefined_check(proplists:get_value("receipt", PostData, undefined), "HintRequestDataError")),
  {success, VUin} = gateway_util:get_uin_by_session(SessionID, iap_buy),
  {success, CacheNode} = gateway_util:get_cache_node(VUin),

  ?FILE_LOG_DEBUG("iap_buy : uin=~p, receipt=~p", [VUin, Receipt]),
  Json = dd_util:encode_json_utf8({struct, [{<<"receipt-data">>, Receipt}]}),
  AppRet =
    case catch httpc:request('post', {"https://buy.itunes.apple.com/verifyReceipt", [], "application/json", iolist_to_binary(Json)}, [], []) of
      {ok, {_StatusLine, _Headers, Body}} ->
        {struct, BodyList} = mochijson2:decode(Body),
        case proplists:get_value(<<"status">>, BodyList, 0) of
          21007 ->
            case catch httpc:request('post',{"https://sandbox.itunes.apple.com/verifyReceipt", [], "application/json", iolist_to_binary(Json)}, [], []) of
              {ok, {_SandBoxStatusLine, _SandBoxHeaders, SandBoxBody}} ->
                {struct, SandBoxBodyList} = mochijson2:decode(SandBoxBody),
                case proplists:get_value(<<"status">>, SandBoxBodyList) of
                  21007 ->
                    ?FILE_LOG_ERROR("sandbox 21007 userid=~p", [VUin]),
                    write_fail_log(CacheNode, VUin, Receipt, "SandBoxBuyReceipt"),
                    throw({custom, "HintPurchaseError"});
                  0 -> SandBoxBodyList;
                  OtherSandBoxStatus ->
                    ?FILE_LOG_ERROR("sandbox exception uin=~p, reason=~p", [VUin, OtherSandBoxStatus]),
                    write_fail_log(CacheNode, VUin, Receipt, "SandBoxBuyReceiptException"),
                    throw({custom, "HintPurchaseError"})
                end;
              SandBoxOther ->
                ?FILE_LOG_ERROR("sandbox exception, resend to sandbox, uin=~p, result=~p.~n",
                  [VUin, SandBoxOther]),
                write_fail_log(CacheNode, VUin, Receipt, "SandBoxBuyReceiptException"),
                throw({custom, "HintPurchaseError"})
            end;
          0 -> BodyList;
          OtherItunesStatus ->
            ?FILE_LOG_ERROR("product exception uin=~p, reason=~p", [VUin, OtherItunesStatus]),
            write_fail_log(CacheNode, VUin, Receipt, "ProductBuyReceiptException"),
            throw({custom, "HintPurchaseError"})
        end;
      Other ->
        ?FILE_LOG_ERROR("hiap itunes exception uin=~p, result=~p.~n", [VUin, Other]),
        write_fail_log(CacheNode, VUin, Receipt, "HIAPItunesException"),
        throw({custom, "HintPurchaseError"})
    end,
  {struct, ReceiptList} = proplists:get_value(<<"receipt">>, AppRet, []),
  ProductID = dd_util:to_list(proplists:get_value(<<"product_id">>, ReceiptList, undefined)),
  Orders = dd_util:to_list(proplists:get_value(<<"original_transaction_id">>, ReceiptList, undefined)),

  case rpc:call(CacheNode, cache, iap_buy, [VUin, Orders, ProductID, Receipt]) of
    {badrpc, Reason1} ->
      ?FILE_LOG_ERROR("iap_buy badrpc error, reason: [~p]", [Reason1]),
      write_fail_log(CacheNode, VUin, Receipt, "BADRPCException"),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason1)}]};
    {success, DataBin} ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, DataBin},
          {<<"sign">>, gateway_util:back_signature(DataBin)}
        ]
      };
    {fail, Reason} ->
      ?FILE_LOG_ERROR("iap_buy error, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.
