%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 十二月 2014 下午7:33
%%%-------------------------------------------------------------------
-module(mod_get_mass_attach_mail).
-author("zqlt").

-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([req_handle/1]).

req_handle(Req) ->
  gateway_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  PostData = Req:parse_post(),
  gateway_util:signature_check(PostData),

  SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
  Type = dd_util:to_integer(gateway_util:undefined_check(proplists:get_value("type", PostData,undefied), "HintRequestDataError")),

  {success, Uin} = gateway_util:get_uin_by_session(SessionID, get_mass_attach_mail),
  {success, CacheNode} = gateway_util:get_cache_node(Uin),

  ?FILE_LOG_DEBUG("get_mass_attach_mail => uin = ~p, session = ~p, type =  ~p", [Uin, SessionID, Type]),

  case rpc:call(CacheNode, cache, get_mass_attach_mail, [Uin, Type]) of
    {success, DataBin} ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, DataBin},
          {<<"sign">>, gateway_util:back_signature(DataBin)}
        ]
      };
    {fail, Reason} ->
      ?FILE_LOG_ERROR("get_login_reward error, reason: [~p]", [Reason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, dd_util:to_binary(Reason)}]}
  end.