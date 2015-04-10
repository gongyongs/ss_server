%%%-------------------------------------------------------------------
%%% @author 95
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 六月 2014 下午5:53
%%%-------------------------------------------------------------------
-module(mod_query_all_bulletin_mail).
-author("95").
-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").
%% API
-export([req_handle/1]).

result_to_erl(BulletinList) ->
	MailList =
		lists:map(
			fun({ID, Title, Content, AddTs}) ->
        {
          struct,
          [
            {<<"id">>, ID},
            {<<"title">>, dd_util:to_binary(Title)},
            {<<"content">>, dd_util:to_binary(Content)},
            {<<"time">>, dd_util:to_binary(dd_util:time_format(dd_util:to_local_time(dd_util:timestamp_to_datetime(AddTs))))}
          ]
        }
			end, BulletinList),
  {struct, [{<<"bulletin">>, MailList}]}.


req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  _GetData = Req:parse_post(),

	?FILE_LOG_DEBUG("mod_query_all_bulletin_mail", []),

  case catch adminserver_cache_proxy:rpc_mail_call(get_bulletin, []) of
    {success, BulletinMailList} ->
    			ResultData = result_to_erl(BulletinMailList),
    			{
    			  struct,
    			  [
    			  	{<<"result">>, 0},
    			  	{<<"data">>, ResultData}
    			  ]
    			};
    {fail, Reason} ->
    	{
        	struct,
       		[
          		{<<"result">>, -1},
          		{<<"error">>, dd_util:to_binary(Reason)}
        	]
      	}
   end.