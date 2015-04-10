%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 九月 2014 上午9:47
%%%-------------------------------------------------------------------
-module(mod_check_device).
-author("zqlt").

-include("../dispatch.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-export([req_handle/1]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  ?FILE_LOG_DEBUG("now in login PostData is --->~p~n",[PostData]),
  Plat = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("plat", PostData, undefined), "plat not exists")),

  PlatType = string:to_lower(Plat),

  ?FILE_LOG_DEBUG("login type = ~p", [PlatType]),

  case PlatType of
    "pp" ->   mod_check_device_pp:req_handle(Req);
    "360" ->  mod_check_device_360:req_handle(Req);
    "tb" -> mod_check_device_tb:req_handle(Req);
    "ky" -> mod_check_device_ky:req_handle(Req);
    "xy" -> mod_check_device_xy:req_handle(Req);
    _ -> mod_check_device_ios:req_handle(Req)
  end.


