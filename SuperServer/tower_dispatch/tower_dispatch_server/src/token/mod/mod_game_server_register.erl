%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 七月 2014 上午10:24
%%%-------------------------------------------------------------------
-module(mod_game_server_register).
-author("zqlt").

-include("../../../deps/file_log/include/file_log.hrl").

%% API
-export([req_handle/1]).

req_handle(Req) ->
  PostData = Req:parse_post(),
  Id = dd_util:to_integer(proplists:get_value("id", PostData, undefined)),
  Name = dd_util:to_list(proplists:get_value("name", PostData, undefined)),
  Type = dd_util:to_list(proplists:get_value("type", PostData, undefined)),
  Ip = dd_util:to_list(proplists:get_value("ip", PostData, undefined)),
  Port = dd_util:to_integer(proplists:get_value("port", PostData, undefined)),
  State = dd_util:to_integer(proplists:get_value("state", PostData, undefined)),
  ?FILE_LOG_DEBUG("id=~p, name=~p, type = ~p, ip=~p, port=~p, state=~p", [Id, Name, Type, Ip, Port, State]),

  game_server_cache:update_game_server(Id, Name, Type, {Ip, Port}, State),

  {struct, [{<<"code">>, 0}]}.

