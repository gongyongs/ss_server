%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 七月 2014 下午5:38
%%%-------------------------------------------------------------------
-ifndef(_DD_MS_HRL__).
-define(_DD_MS_HRL__, true).
-record(dispatch_info, {}).
-record(dispatch, {
  node :: node(),
  info :: #dispatch_info{}
}).


-record(login_info, {}).
-record(login, {node :: node(), info :: #login_info{}}).

-record(global_config, {key :: term(), value :: term()}).

-endif.
