%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 七月 2014 下午8:58
%%%-------------------------------------------------------------------
-module(mod_token_verify).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../../dispatch/dispatch.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  PostData = Req:parse_post(),
  Uin = dd_util:to_integer(proplists:get_value("uin", PostData, undefined)),
  Token = dd_util:to_list(proplists:get_value("token", PostData, undefined)),

  ?FILE_LOG_DEBUG("uin = ~p, token = ~p", [Uin, Token]),

  case token_cache:get_login_token(Uin) of
      {success, {Token, LoginInfo}} ->
        Data =
          {
            struct,
            [
              {<<"ip">>, dd_util:to_binary(LoginInfo#login_user_info.ip)},
              {<<"device">>, dd_util:to_binary(LoginInfo#login_user_info.device)},
              {<<"plat_type">>, dd_util:to_binary(LoginInfo#login_user_info.plat_type)},
              {<<"plat_dis_name">>, dd_util:to_binary(LoginInfo#login_user_info.plat_dis_name)},
              {<<"plat_id">>, dd_util:to_binary(LoginInfo#login_user_info.plat_id)}
            ]
          },
        {struct, [{<<"code">>, 0}, {<<"data">>, Data}]};
      {success, {TmpToken, _}} ->
        ?FILE_LOG_DEBUG("cur_token=~p, tmp_token=~p", [Token, TmpToken]),
        {struct, [{<<"code">>, -1}, {<<"error">>, dd_util:to_binary("HintSystemError")}]};
      fail ->
        {struct, [{<<"code">>, -2}, {<<"error">>, dd_util:to_binary("HintSystemError")}]};
      Other ->
        ?FILE_LOG_WARNING("other=~p", [Other]),
        {struct, [{<<"code">>, -3}, {<<"error">>, dd_util:to_binary("HintSystemError")}]}
  end.




