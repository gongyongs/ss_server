%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午5:09
%%%-------------------------------------------------------------------
-module(platform_pay_util).
-author("zqlt").

%% API
-export([
  check_plat/1
]).

check_plat(PlatType) ->
  case PlatType of
    "pp" -> ok;
    "360" -> ok;
    "ky" -> ok;
    "tb" -> ok;
    _ -> throw({custom, "HintRequestDataError"})
  end.

