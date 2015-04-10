%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 2015年3月9号上午9点55分
%%%-------------------------------------------------------------------
-module(mod_gm).
-author("zqlt").
-include("../../../deps/file_log/include/file_log.hrl").
-include("../gateway.hrl").
-include("../../cache/cache_def.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([req_handle/1]).

req_handle(Req) ->
	gateway_util:false_check((Req:get(method) =:= ?POST), "request method error"),
  	PostData = Req:parse_post(),
  	gateway_util:signature_check(PostData),
	
	?FILE_LOG_DEBUG("gm post data = ~p", [PostData]),

  	SessionID = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("session_id", PostData, undefined), "HintRequestDataError")),
	Action = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("action", PostData, undefined), "HintRequestDataError")),
	OP = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("op", PostData, undefined), "HintRequestDataError")),
	Params = dd_util:to_list(gateway_util:undefined_check(proplists:get_value("param", PostData, undefined), "HintRequestDataError")),
	
	?FILE_LOG_DEBUG("gm => ~p, ~p", [SessionID, OP]),
  	{success, Uin} = gateway_util:get_uin_by_session(SessionID, get_user_info),
  	{success, CacheNode} = gateway_util:get_cache_node(Uin),
	Ip = Req:get(peer),
	RPCRESULT = 
	case Action of
		"op_bag"->
			case OP of
				"get"-> 
					rpc:call(CacheNode, cache, op_bag, [Uin,OP,Params]);
				"add"->
					{struct, [{<<"paramlist">>, [ItemID,Count]}]} = mochijson2:decode(Params),
					rpc:call(CacheNode, cache, op_bag_add, [Uin,ItemID,Count,Ip]);
				"del"-> 
					{struct, [{<<"paramlist">>, [ItemID,Count]}]} = mochijson2:decode(Params),
					rpc:call(CacheNode, cache, op_bag_del, [Uin,ItemID,Count,Ip]);
				"test"->
					{struct, [{<<"paramlist">>, [ItemID,Count]}]} = mochijson2:decode(Params),
						rpc:call(CacheNode, cache, op_bag_del, [Uin,ItemID,Count,Ip])
			end;
		_->
			ok
	end,
	case RPCRESULT of
		ok->
			ok;
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
	      ?FILE_LOG_ERROR("op_bag error, reason = ~p", [FailReason]),
	      {struct, [{<<"result">>, -1}, {<<"error">>, <<"HintSystemDataError">>}]}
  	end.