%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2015 上午10:57
%%%-------------------------------------------------------------------
-author("zqlt").

-record(rd_order_tb, {
  bill_no::string(),
  user_id::string(),
  goods_id::string(),
  uin::integer(),
  state::integer(),  %%0： 已发货， 1.未处理， 2: 出现异常，10: 订单存在问题。
  order_price,       %%以字符串存储的float型  人民币
  update_info,       %%用户保存，发货之后，玩家更新的数据，供客户端查询
  time_stamp::integer()
}).

-define(APP_ID_TB, 150124).
-define(APP_KEY_TB, "E7Qd1qANkZH0UJ5tDQnd1AXMZwH0Jg4t").