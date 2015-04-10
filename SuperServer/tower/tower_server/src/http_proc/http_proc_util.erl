%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 十一月 2014 下午8:29
%%%-------------------------------------------------------------------
-module(http_proc_util).
-author("zqlt").
-include("../dd_ms.hrl").
-include("../../deps/file_log/include/file_log.hrl").
%% API
-export([
  get_pay_cb_notify_by_path/1,
  get_pay_cb_notify_by_mod/1,
  get_cache_node/1
]).

-export([
  undefined_check/2,
  false_check/2,
  empty_check/2,
  base64_decode/1,
  get_json_value/2,
  eksort/1
]).

get_cache_node(Uin) when is_integer(Uin)->
  {success, CacheHashRule} = dd_ms:read_config(cache_hash_rule),
  case hash_service_util:find_key_store_node(dd_util:to_list(Uin), CacheHashRule) of
    fail ->
      ?FILE_LOG_ERROR("no available data node, ~p", [Uin]),
      throw({custom, "HintSystemError"});
    {success, Node} when is_atom(Node) -> {success, Node}
  end.


get_pay_cb_notify_by_path(Path) when is_list(Path) ->
  {success, Handles} = dd_config:get_cfg(call_back_notify),
  handle_item_by_path(Path, Handles).


get_pay_cb_notify_by_mod(Mod) when is_atom(Mod) ->
  {success, Handles} = dd_config:get_cfg(call_back_notify),
  handle_item_by_mod(Mod, Handles).


handle_item_by_path(_, []) -> fail;
handle_item_by_path(Path, [{_, Path, _} = H | _T]) ->  {success, H};
handle_item_by_path(Path, [_| T]) -> handle_item_by_path(Path, T).



handle_item_by_mod(_, []) -> fail;
handle_item_by_mod(Mod, [{Mod, _, _} = H | _T]) ->  {success, H};
handle_item_by_mod(Mod, [_| T]) -> handle_item_by_mod(Mod, T).


undefined_check(Value, Reason) ->
  param_check(Value, undefined, Reason).

false_check(Value, Reason) ->
  param_check(Value, false, Reason).

empty_check(Value, Reason) ->
  param_check(Value, [], Reason).


param_check(Value, Value, Reason) ->
  throw({custom, Reason});
param_check(Value, _, _) -> Value.


base64_decode(Value) ->
  base64:decode(Value).

get_json_value(Key, PropList) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      throw({custom, "HintSystemError"});
    Value -> Value
  end.

eksort(P) ->
  Fun =
    fun({LKey, _}, {RKey, _}) ->
      compare(LKey, RKey)
    end,
  lists:sort(Fun, P).

compare([], []) -> true;
compare([], _) -> true;
compare(_, []) -> false;
compare([L|_], [R|_]) when L > R -> false;
compare([L|_], [R|_]) when L < R -> true;
compare([L|T1], [L|T2]) -> compare(T1, T2).



