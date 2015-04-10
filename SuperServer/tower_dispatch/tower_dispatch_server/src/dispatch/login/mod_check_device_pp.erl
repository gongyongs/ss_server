%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午4:12
%%%-------------------------------------------------------------------
-module(mod_check_device_pp).
-author("zqlt").
-include("../dispatch.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-export([req_handle/1]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  Device = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("dev", PostData, undefined), "device not exists")),
  Version = dd_util:to_list(dispatch_util:undefined_with_default_value(proplists:get_value("version", PostData, undefined), "")),


  ?FILE_LOG_DEBUG("pp_check device => device = ~p, version = ~p", [Device, Version]),

  dispatch_util:check_string_valid(Version),

  {success, TokenNode} = dispatch_util:get_config_param(token_node),

  case rpc:call(TokenNode, token_cache, get_version_info, [Version]) of
    {success, VersionInfo} ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"code">>, 0},
          {<<"version_update_url">>, dd_util:to_binary(VersionInfo#version.version_package_url)},
          {<<"version_url">>, dd_util:to_binary(VersionInfo#version.version_update_url)}
        ]
      };
    Other ->
      ?FILE_LOG_ERROR("get version info error, reason = ~p", [Other]),
      {
        struct,
        [
          {<<"result">>, -1},
          {<<"error">>, <<"HintSystemError">>}
        ]
      }
  end.
