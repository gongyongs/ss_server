%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午3:10
%%%-------------------------------------------------------------------
-author("zqlt").

-record(rd_order_360, {
  bill_no::string(),
  user_id::string(),
  goods_id::string(),
  uin::integer(),
  state::integer(),  %%0： 已发货， 1.未处理， 2: 出现异常，10: 订单存在问题。
  order_price,
  update_info,       %%用户保存，发货之后，玩家更新的数据，供客户端查询
  time_stamp::integer()
}).

-define(APP_ID_360, 202219501).
-define(APP_KEY_360, "abcb5a7ab165ad4d356b6e9194690430").
-define(APP_SECRET_360, "2ea259f2060af1b9c451974f978993b7").
