%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十二月 2014 下午9:56
%%%-------------------------------------------------------------------
-author("zqlt").

-record(cdkey_rd,{
  code_id::string(),        %%激活码id
  code_type::integer(),     %%激活码类型：1. 单平台使用  0. 全平台使用
  plat_type::string(),      %%平台类型：PP: pp平台，KY: 快用平台 GT: 怪唐平台  "": 其他平台
  auth_type::integer(),       %%使用权限：0 全部账户使用 1, vip账户
  code_content::string(),       %%激活码内容ID
  create_ts::integer(),           %%创建时间戳
  expiration_ts::integer(),     %%过期时间戳
  status::integer(),             %%状态：0 未兑换  1, 已兑换  2 已强制过期
  exchange_ts::integer()          %%兑换时间
}).
