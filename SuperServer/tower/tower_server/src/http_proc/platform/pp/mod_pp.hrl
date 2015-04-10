%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 十一月 2014 上午11:42
%%%-------------------------------------------------------------------
-author("zqlt").

-record(rd_order_pp, {
  bill_no::string(),
  user_id::string(),
  goods_id::string(),
  uin::integer(),
  state::integer(),  %%0： 已发货， 1.未处理， 2: 出现异常，10: 订单存在问题。
  order_price,
  update_info,       %%用户保存，发货之后，玩家更新的数据，供客户端查询
  time_stamp::integer()
}).

-define(APP_ID_PP, 4931).
-define(APP_KEY_PP, "7e228763ade8aba96e28c393a076c525").

