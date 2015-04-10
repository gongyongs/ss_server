%% @author zqlt
%% @doc @todo Add description to mod_op_rewardmatch.
-module(mod_op_rewardmatch).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([req_handle/1]).
-export([]).

req_handle(Req) ->
	gateway_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  	PostData = Req:parse_post(),
  	gateway_util:signature_check(PostData),
	
	?FILE_LOG_DEBUG("op bag post data = ~p", [PostData]),

  	SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
	OP = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("op", PostData, undefined), "HintRequestDataError")),
	Params = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("param", PostData, undefined), "HintRequestDataError")),
	
	
	?FILE_LOG_DEBUG("op_bag => ~p, ~p", [SessionID, OP]),
  	{success, Uin} = gateway_util:get_uin_by_session(SessionID, op_rewardmatch),
  	{success, CacheNode} = gateway_util:get_cache_node(Uin),
	
	_Ip = Req:get(peer),
	
	%NewList = lists:map(fun(Type) -> dd_util:to_list(Type) end, TypeList),
	%UinList = lists:map(fun(ID) -> dd_util:to_integer(ID) end, IDList),

	RPCRESULT = rpc:call(CacheNode, cache, op_rewardmatch, [Uin,OP,Params]),
	
	%case rpc:call(CacheNode, cache, op_bag, [Uin,OP,Params]) of
	case RPCRESULT of
    {success, DataBin} ->
      {
        struct,
        [
          {<<"result">>, 0},
          {<<"data">>, DataBin},
          {<<"sign">>, gateway_util:back_signature(DataBin)}
        ]
      };
    FailReason ->
      ?FILE_LOG_ERROR("op_rewardmatch error, reason = ~p", [FailReason]),
      {struct, [{<<"result">>, -1}, {<<"error">>, <<"HintSystemDataError">>}]}
  	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


