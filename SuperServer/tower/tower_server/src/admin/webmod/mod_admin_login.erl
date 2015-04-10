-module(mod_admin_login).
-author(j).
-export([req_handle/1]).
-export([generate_oper_code/1]).

-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API



admin_list() ->
  [
    {"longtugame", "123456"}
  ].

admin_verify(UName, Pwd) ->          %%辨别用户名和密码
  case proplists:get_value(UName, admin_list(), undefined) of
    undefined -> {fail, "account not exist!!"};
    Pwd ->  success;
    _ -> {fail, "password error!!"}
  end.

generate_oper_code(UserName) ->     %%生成oper_code
  UserName ++ dd_util:to_list(dd_util:timestamp()).

req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),

  UName = dd_util:to_list(proplists:get_value("admin_name", GetData, undefined)),
  Pwd = dd_util:to_list(proplists:get_value("admin_pwd", GetData, undefined)),
  ?FILE_LOG_DEBUG("mod_admin_login=> admin_name=~p, admin_pwd=~p.", [UName, Pwd]),
  case admin_verify(UName, Pwd) of
    {fail, Reason} ->
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"error">>, dd_util:to_binary(Reason)}
        ]
      };
    success ->
      OperCode = generate_oper_code(UName),
      adminserver_cache_proxy:add_admin(OperCode, UName),
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"oper_code">>, dd_util:to_binary(OperCode)}
        ]
      }
  end.




