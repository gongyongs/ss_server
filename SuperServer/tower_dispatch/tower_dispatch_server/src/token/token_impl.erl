%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 七月 2014 上午11:16
%%%-------------------------------------------------------------------
-module(token_impl).
-author("zqlt").

-include("../../deps/file_log/include/file_log.hrl").
-include("token.hrl").
%% API
-export([execute/2]).


execute(add_login_token, {Uin, Token, LoginInfo}) ->
  ?FILE_LOG_DEBUG("add_login_token[~p, ~p, ~p]", [Uin, Token, LoginInfo]),
  TableID = get(table_id),
  ets:insert(TableID, #token_rd{uin = Uin, token = Token, add_time = dd_util:timestamp(), login_info = LoginInfo}),
  success;

execute(get_login_token, {Uin, IsDelete}) ->
  ?FILE_LOG_DEBUG("get_login_token [~p]", [Uin]),
  TableID = get(table_id),

  RetValue =
    case ets:lookup(TableID, Uin) of
      [] -> fail;
      [#token_rd{token = Token, login_info = LoginInfo}] ->
        if
          IsDelete =:= true ->
            ets:delete(TableID, Uin);
          true -> ok
        end,
        {success, {Token, LoginInfo}}
    end,

  RetValue.
