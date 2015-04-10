%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2014 17:46
%%%-------------------------------------------------------------------
-module(mod_add_shop_item).
-author(j).
-export([req_handle/1]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../csv.hrl").
%% API

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  PostData = Req:parse_post(),

  OperCode =
    try
      dd_util:to_list(proplists:get_value("oper_code", PostData, undefined))
    catch
      _:_ ->
        Ret0 = {struct,[{<<"result">>, -1},{<<"error">>, <<"request param error">>}]},
        throw({custom, Ret0})
    end,

  UserName =
    case adminserver_cache_proxy:get_admin(OperCode) of
      fail ->
        Ret1 = {struct,[{<<"result">>, -1},{<<"error">>, <<"Illegal Operation, Please login again">>}]},
        throw({custom, Ret1});
      {success, AdminUName} -> AdminUName;
      _Other ->
        Ret2 = {struct,[{<<"result">>, -1},{<<"error">>, <<"system error">>}]},
        throw({custom, Ret2})
    end,
  ?FILE_LOG_DEBUG("mod_add_shop_item => oper_code=~p, uname=~p.", [OperCode, UserName]),

  Shop_id = dd_util:to_list(adminserver_util:undefined_check((proplists:get_value("shop_id", PostData, undefined)),"there is no shop_id")),
  Item_name_id = dd_util:to_list(proplists:get_value("item_name_id", PostData, undefined)),
  Load_pic_name_id = dd_util:to_list(proplists:get_value("load_pic_name_id", PostData, undefined)),
  Ttem_id = dd_util:to_list(proplists:get_value("item_id", PostData, undefined)),
  Buy_type_id = dd_util:to_integer(proplists:get_value("buy_type_id", PostData, undefined)),
  Buy_num_id = dd_util:to_integer(proplists:get_value("buy_num_id", PostData, undefined)),
  Use_type_id = dd_util:to_integer(proplists:get_value("use_type_id", PostData, undefined)),
  Use_num_id = dd_util:to_integer(proplists:get_value("use_num_id", PostData, undefined)),
  Gift_type_id = dd_util:to_integer(proplists:get_value("gift_type_id", PostData, undefined)),
  Gift_num_id = dd_util:to_integer(proplists:get_value("gift_num_id", PostData, undefined)),
  Restriction_id = dd_util:to_integer(proplists:get_value("restriction_id", PostData, undefined)),
  Recommended_id = dd_util:to_integer(proplists:get_value("recommended_id", PostData, undefined)),
  Time_start_id = dd_util:to_integer(proplists:get_value("time_start_id", PostData, undefined)),
  Time_stop_id = dd_util:to_integer(proplists:get_value("time_stop_id", PostData, undefined)),

  Item = #res_goods{
    id = Shop_id,
    name = Item_name_id,
    pic = Load_pic_name_id,
    goods_no = Ttem_id,
    goods_type = Buy_type_id,
    goods_count = Buy_num_id,
    money_type = Use_type_id,
    money_count = dd_util:to_list(Use_num_id),
    gift_type = Gift_type_id,
    gift_count = Gift_num_id,
    restrict_count = Restriction_id,
    is_recommend = Recommended_id,
    start_ts = Time_start_id,
    over_ts = Time_stop_id
  },
  ?FILE_LOG_DEBUG("Record Item:~p",[Item]),

  case adminserver_db:add_shop_item(Item) of           %添加商城物品
    success->
    {
      struct,
      [
        {<<"result">>, 0}
      ]
    };
    {fail, Reason} ->
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"error">>, dd_util:to_binary(Reason)}
        ]
      }
  end.
