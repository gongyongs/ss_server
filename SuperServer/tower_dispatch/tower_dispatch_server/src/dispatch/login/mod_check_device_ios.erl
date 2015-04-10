%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2015 下午4:12
%%%-------------------------------------------------------------------
-module(mod_check_device_ios).
-author("zqlt").

-include("../dispatch.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
-export([req_handle/1]).

req_handle(Req) ->
  dispatch_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  Device = dd_util:to_list(dispatch_util:undefined_check(proplists:get_value("dev", PostData, undefined), "device not exists")),
  Version = dd_util:to_list(dispatch_util:undefined_with_default_value(proplists:get_value("version", PostData, undefined), "")),


  ?FILE_LOG_DEBUG("check device => device = ~p, version = ~p", [Device, Version]),

  {success, Node} = dispatch_util:get_login_node(Device),

  dispatch_util:check_string_valid(Version),

  {success, TokenNode} = dispatch_util:get_config_param(token_node),

  VersionInfo =
    case rpc:call(TokenNode, token_cache, get_version_info, [Version]) of
      {success, Info} -> Info;
      Other ->
        ?FILE_LOG_ERROR("get version info error, reason = ~p", [Other]),
        throw({custom, "HintSystemError"})
    end,

  case rpc:call(Node, login_cache, check_device, [Device]) of
    {success, Code} ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"code">>, Code},
          {<<"version_update_url">>, dd_util:to_binary(VersionInfo#version.version_package_url)},
          {<<"version_url">>, dd_util:to_binary(VersionInfo#version.version_update_url)}
        ]
      };
    {fail, Reason} ->
      ?FILE_LOG_ERROR("check device, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.
