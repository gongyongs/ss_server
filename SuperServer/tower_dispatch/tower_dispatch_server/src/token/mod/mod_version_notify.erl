%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 十二月 2014 下午5:03
%%%-------------------------------------------------------------------
-module(mod_version_notify).
-author("zqlt").

-include("../../../deps/file_log/include/file_log.hrl").
-include("../../dispatch/dispatch.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  PostData = Req:parse_post(),
  VersionInfoJson = dd_util:to_list(proplists:get_value("version_info", PostData, undefined)),
  ?FILE_LOG_DEBUG("version info notify => version = ~p", [VersionInfoJson]),

  {success, Version} = decode_version_info(VersionInfoJson),
  ?FILE_LOG_DEBUG("version info = ~p", [Version]).

  %%版本信息的热更新地址修改
%%   case token_cache: of
%%     {success, List} ->
%%       FriendDataJsonList = game_server_util:encode_json_request_friend_data(List),
%%       {
%%         struct,
%%         [
%%           {<<"result">>, 0},
%%           {<<"data">>, FriendDataJsonList}
%%         ]
%%       };
%%     {fail, Reason} ->
%%       {
%%         struct,
%%         [
%%           {<<"result">>, -1},
%%           {<<"reason">>, dd_util:to_binary(Reason)}
%%         ]
%%       }
%%   end.

decode_version_info(VersionInfo) ->
  {struct, VersionInfoList} = mochijson2:decode(VersionInfo),
  VersionID = dd_util:to_list(get_json_value(<<"version_id">>, VersionInfoList)),
  VersionPackageUrl = dd_util:to_list(get_json_value(<<"package_url">>, VersionInfoList)),
  VersionUrl = dd_util:to_list(get_json_value(<<"version_url">>, VersionInfoList )),
  {success, #version{version_id = VersionID, version_package_url = VersionPackageUrl, version_update_url = VersionUrl}}.

get_json_value(Key, PropList) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      throw({custom, "error json key" ++ dd_util:to_list(Key)});
    Value -> Value
  end.