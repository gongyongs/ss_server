%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 一月 2015 下午5:27
%%%-------------------------------------------------------------------
-module(mod_ky).
-author("2q123").
-include("mod_ky.hrl").
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
  mod_ky_ms:query_order(UserID, BillNo).

generate_orders(UserID, GoodsID, Uin) when is_list(UserID) andalso is_list(GoodsID) andalso is_integer(Uin) ->
  ?FILE_LOG_DEBUG("generate_order => userid = ~p, goodid = ~p, uin = ~p", [UserID, GoodsID, Uin]),
  mod_ky_ms:generate_orders(UserID, GoodsID, Uin).

call_back_handle(Req) ->
  PostData = Req:parse_post(),
  ?FILE_LOG_DEBUG("post = ~p", [PostData]),
  try
    NotifyData = dd_util:to_list(http_proc_util:undefined_check(proplists:get_value("notify_data", PostData, undefined), "notify_data not exist")),
    OrderID = dd_util:to_list(http_proc_util:undefined_check(proplists:get_value("orderid", PostData, undefined), "order_id not exist")),
    BillNo = dd_util:to_list(http_proc_util:undefined_check(proplists:get_value("dealseq", PostData, undefined), "bill_no not exist")),
    AccountID = dd_util:to_list(http_proc_util:undefined_check(proplists:get_value("uid", PostData, undefined), "account not exist")),
    Sign = dd_util:to_list(http_proc_util:undefined_check(proplists:get_value("sign", PostData, undefined), "sign not exist")),
    Subject = dd_util:to_list(http_proc_util:undefined_check(proplists:get_value("subject", PostData, undefined), "subject not exist")),
    {success, Key} = mod_ky_ms:read_public_key(),

    ?FILE_LOG_DEBUG("call back: notify = ~p, orderid = ~p, billno = ~p, uid = ~p, sign = ~p", [NotifyData, OrderID, BillNo, AccountID, Sign]),

    {SignBillNo, Fee, PayResult} = decode_notify_data(http_proc_util:base64_decode(NotifyData), Key),
    ?FILE_LOG_DEBUG("decode notify data = ~p, ~p, ~p", [SignBillNo, Fee, PayResult]),

    true = (SignBillNo =:= BillNo),

    P1 = proplists:delete("sign", PostData),
    P2 = http_proc_util:eksort(P1),
    TmpSign = dd_util:to_binary(joint_value(P2, "")),
    ?FILE_LOG_DEBUG("temp sign = ~s", [unicode:characters_to_binary(TmpSign)]),
    SignResult = verify_sign(Key, TmpSign, http_proc_util:base64_decode(Sign)),
    mod_ky_ms:complete_orders(OrderID, BillNo, AccountID, Fee, Subject, PayResult, SignResult)
  catch
    What:Type ->
      ?FILE_LOG_ERROR("what = ~p, type = ~p, stack = ~p, postdata = ~p", [What, Type, erlang:get_stacktrace(), PostData]),
      <<"failed">>
  end.


verify_sign(PubKey, Data, Sign) ->
  try
    ?FILE_LOG_DEBUG("public key = ~p, data = ~p, sign = ~p", [PubKey, Data, Sign]),
    public_key:verify(Data, 'sha', Sign, PubKey)
  catch
    What:Type ->
      ?FILE_LOG_ERROR("verify sign what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      false
  end.

decode_notify_data(Data, PubKey) ->
  Bin = public_key:decrypt_public(Data, PubKey),
  DecrData = dd_util:to_list(Bin),
  List = string:tokens(DecrData, "&"),
  DataList = lists:map(fun(Item) -> [Key, Value] = string:tokens(Item, "="), {Key, Value} end, List),
  DealSeq = proplists:get_value("dealseq", DataList),
  Fee = proplists:get_value("fee", DataList),
  PayResult = proplists:get_value("payresult", DataList),
  {dd_util:to_list(DealSeq), dd_util:to_list(Fee), dd_util:to_integer(PayResult)}.


joint_value([], R) -> R;
joint_value([{K,V}|T], "") -> joint_value(T, K ++ "=" ++ V);
joint_value([{K,V}|T], R) -> joint_value(T, R ++ "&" ++ K ++ "=" ++ V).



