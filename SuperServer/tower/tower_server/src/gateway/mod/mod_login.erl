%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 七月 2014 下午2:22
%%%-------------------------------------------------------------------
-module(mod_login).
-author("zqlt").

-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  Uin = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("uin", PostData, undefined), "HintRequestDataError")),
  Token = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("token", PostData, undefined), "HintRequestDataError")),

  gateway_util:signature_check(PostData),

  ?FILE_LOG_DEBUG("login post data = ~p", [PostData]),

  ?FILE_LOG_DEBUG("login => request value : uin = ~p, token = ~p", [Uin, Token]),
  %%验证token正确性
  {success, {DispatchSeverIp, DispatchServerPort}} = dd_ms:read_config(dispatch_server),
  Value = lists:flatten(["uin=", dd_util:to_list(Uin), "&token=", Token]),
  ReqUrl = lists:flatten(["http://", DispatchSeverIp, ":" , dd_util:to_list(DispatchServerPort), "/token_verify"]),
  ?FILE_LOG_DEBUG("login => token verifiy : request url = ~p", [ReqUrl]),
  {Ip, Device, PlatType, PlatID, PlatDisName} =
    case httpc:request('post', {ReqUrl, [], "application/x-www-form-urlencoded",iolist_to_binary(Value)}, [], []) of
      {ok, {_Status, _Header, Body}} ->
        {struct, JsonList} = mochijson2:decode(Body),
        Code = dd_util:to_integer(gateway_util:get_json_value(<<"code">>, JsonList)),
        if
          Code =:= 0 ->
            {struct, DataList} = gateway_util:get_json_value(<<"data">>, JsonList),
            {
              dd_util:to_list(gateway_util:get_json_value(<<"ip">>, DataList)),
              dd_util:to_list(gateway_util:get_json_value(<<"device">>, DataList)),
              gateway_util:get_plat_type(dd_util:to_list(gateway_util:get_json_value(<<"plat_type">>, DataList))),
              dd_util:to_list(gateway_util:get_json_value(<<"plat_id">>, DataList)),
              dd_util:to_list(gateway_util:get_json_value(<<"plat_dis_name">>, DataList))
            };
          true ->
            ?FILE_LOG_ERROR("login => token verify http request error, errorcode = ~p", [Code]),
            throw({custom, "HintReLogin"})
        end;
      Other ->
        ?FILE_LOG_ERROR("login => token verify http request error, reason = ~p", [Other]),
        throw({custom, "HintReLogin"})
    end,


  {success, SessionNode} = dd_ms:read_config(session_node),
  SessionValue =
    try
      case rpc:call(SessionNode, session_work, login, [Uin, node(), Ip, Device]) of
        {badrpc, Reason} -> throw({custom, Reason});
        {success, SessionID} -> SessionID;
        OtherValue ->
          ?FILE_LOG_ERROR("other ~p", [OtherValue]),
         throw({custom, "HintSystemError"})
      end
    catch
      What:Type ->
        ?FILE_LOG_ERROR("what=~p, type=~p, stack=~p", [What, Type, erlang:get_stacktrace()]),
        throw({custom, "HintReLogin"})
    end,

  LoginInfo = {Ip, Device, PlatType, PlatID, PlatDisName},
  ?FILE_LOG_DEBUG("login token verify result = ~p", [LoginInfo]),

  {success, CacheNode} = gateway_util:get_cache_node(Uin),
  {success, DataBin} = rpc:call(CacheNode, cache, login, [Uin, LoginInfo]),
  {struct, _DataJsonList} = mochijson2:decode(DataBin),
  Data = {struct, [{<<"session">>, dd_util:to_binary(SessionValue)}]},
  DataJson = dd_util:encode_json_utf8(Data),

  {
    struct,
    [
      {<<"result">>, 0},
      {<<"data">>, dd_util:to_binary(DataJson)},
      {<<"sign">>, gateway_util:back_signature(DataJson)}
    ]
  }.

