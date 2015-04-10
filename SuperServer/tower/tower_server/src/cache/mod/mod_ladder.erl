%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 三月 2015 14:26
%%%-------------------------------------------------------------------
-module(mod_ladder).
-author("zqlt").

%% API
-export([]).
-export([execute/2]).

-include("../../../deps/file_log/include/file_log.hrl").
-include("../../mail/mail.hrl").
-include("../../ranking/ranking.hrl").
-include("../../csv.hrl").
-include("../../dd_ms.hrl").
-include("../cache_def.hrl").
-include("../cache_cdkey.hrl").
-include("../protobuf/f_t_time_pb.hrl").


%获取信息
execute("get_info", {Account, _Params}) ->
  {Account, ["reward_match"], #f_t_curtime{time = "123"} }.