%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 七月 2014 下午5:52
%%%-------------------------------------------------------------------
-module(mod_login).
-author("zqlt").

-include("../dispatch.hrl").
-include("../../login/login.hrl").
-include("../../../deps/file_log/include/file_log.hrl").

%% API
-export([req_handle/1]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  Plat = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("plat", PostData, undefined), "plat not exists")),

  PlatType = string:to_lower(Plat),

  ?FILE_LOG_DEBUG("login type = ~p", [PlatType]),

  case PlatType of
    "pp" ->   mod_login_pp:req_handle(Req);
    "360" ->  mod_login_360:req_handle(Req);
    "ky" -> mod_login_ky:req_handle(Req);
    "tb" -> mod_login_tb:req_handle(Req);
    "xy" -> mod_login_xy:req_handle(Req);
    _ -> mod_login_ios:req_handle(Req)
  end.