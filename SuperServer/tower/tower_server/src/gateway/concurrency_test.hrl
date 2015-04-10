%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 十二月 2014 下午2:23
%%%-------------------------------------------------------------------
-author("zqlt").

-record(request_rd,{
  request_id::string(),
  request_time::string(),
  request_start_tms::integer(),
  request_over_tms::integer(),
  response_result::integer(),
  request_cost::integer()
}).

-record(request_info,{
  request_id::string(),
  request_list::[#request_rd{}]
}).
