%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 七月 2014 下午5:38
%%%-------------------------------------------------------------------
-author("zqlt").

%%描述gateway服务的当前信息
-record(gateway_addr, {ip :: string(), port :: integer()}).


-record(gateway_info, {
  cur_user_count=0 :: integer(),
  max_user_count=0 :: integer(),
  gateway_addr :: #gateway_addr{}
}).

-record(gateway, {
  node :: node(),
  info=#gateway_info{} :: #gateway_info{}
}).


-record(cache_info, {}).
-record(cache, {node :: node(), info :: #cache_info{}}).

-record(global_config, {key :: term(), value :: term()}).
