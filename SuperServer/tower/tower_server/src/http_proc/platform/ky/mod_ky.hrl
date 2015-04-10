%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 一月 2015 下午5:27
%%%-------------------------------------------------------------------
-author("zqlt").

-record(rd_order_ky, {
  bill_no::string(),
  user_id::string(),
  goods_id::string(),
  uin::integer(),
  state::integer(),  %%0： 已发货， 1.未处理， 2: 出现异常，10: 订单存在问题。
  order_price, %%以字符串存储的float型  人民币
  update_info,       %%用户保存，发货之后，玩家更新的数据，供客户端查询
  time_stamp::integer()
}).

-define(APP_ID_KY, "com.ogresugar.yumsaga.ky").
-define(APP_KEY_KY, "28e97502a7851eef3047582b3bf087f8").