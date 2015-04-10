%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mod_recompile).
-author("zqlt").


%% ====================================================================
%% API functions
%% ====================================================================
-export([req_handle/1]).
-export([my_exec/1]).
-include("../adminserver.hrl").
-include("../../../deps/file_log/include/file_log.hrl").


req_handle(Req) ->
  Method = Req:get(method), true = (Method =:= ?POST),
  GetData = Req:parse_post(),

  OperCode =
    try
      dd_util:to_list(proplists:get_value("oper_code", GetData, undefined))
    catch
      _:_ ->
        Ret0 = {struct,[{<<"result">>, -1},{<<"error">>, <<"request param error">>}]},
        throw({custom, Ret0})
    end,

  _UserName =
    case adminserver_cache_proxy:get_admin(OperCode) of
      fail ->
        Ret1 = {struct,[{<<"result">>, -1},{<<"error">>, <<"Illegal Operation, Please login again">>}]},
        throw({custom, Ret1});
      {success, AdminUName} -> AdminUName;
      _Other ->
        Ret2 = {struct,[{<<"result">>, -1},{<<"error">>, <<"system error">>}]},
        throw({custom, Ret2})
    end,
  %%调用外部emake编译
  Result = my_exec("base/compile"),
  case Result of
	  success->
		  {struct, [{<<"result">>, 0}]};
	  _->
		  {struct,[{<<"result">>,-1}]}
  end.


my_exec(Command) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    Result = get_data(Port, []),
	case Result of
		{0,_ReList}->
			success;
		_->
			fail
	end.

get_data(Port, Sofar) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
        {'EXIT',  Port,  _} ->
            ok
        after 1 ->              
            ok
        end,
        ExitCode =
            receive
            {Port, {exit_status, Code}} ->
                Code
        end,
        {ExitCode, lists:flatten(Sofar)}
    end.