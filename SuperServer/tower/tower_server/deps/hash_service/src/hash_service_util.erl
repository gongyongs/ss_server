-module(hash_service_util).
-author("123").
%% API
-include("hash_service.hrl").
-export([ensure_app_started/1]).

-export([hex_to_big/1, hex_to_small/1]).

-export([
  hash_string/1
]).
-export([update_hash_rule/2, update_hash_rule/3]).
-export([find_key_store_node/2]).

-spec ensure_app_started(App :: atom()) -> ok.
ensure_app_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end.

hex_to_big(C) when C >= 97 andalso C =< 122 ->  C - 32;
hex_to_big(C) -> C.

hex_to_small(C) when C >= 65 andalso C =< 90 ->  C + 32;
hex_to_small(C) -> C.

hash_string(Value) ->
  hash_string(Value, 1).

hash_string(Value, HashType) ->
  string_iter(Value, 16#7FED7FED, 16#EEEEEEEE, HashType).

string_iter([], S1, _, _) -> S1;
string_iter([V|T], S1, S2, HashType) ->
  Ch = hash_service_util:hex_to_big(V),
  Index = (HashType bsl 8) + Ch,
  [#hash_rd{value = HashValue}] = ets:lookup(hash_rd, Index),
  S1_2 = HashValue bxor (S1 + S2),
  S2_2 = Ch + S1_2 + S2 + (S2 bsl 5) + 3,
  string_iter(T, S1_2, S2_2, HashType).

%%一致哈希性算法
%%主要解决多个缓存问题

to_virtual_node(Node, VirtualIndex) when is_list(Node) andalso is_integer(VirtualIndex) ->
  Node ++ "#" ++ integer_to_list(VirtualIndex).

to_node(VirtualNodeName) ->
  [NodeName, _VirtualIndex] = string:tokens(VirtualNodeName, "#"),
  list_to_atom(NodeName).

%%新增节点
update_hash_rule(Node, HashRule) when is_atom(Node) andalso is_record(HashRule, hash_rule)->
  update_hash_rule(atom_to_list(Node), HashRule);
update_hash_rule(Node, HashRule) when is_list(Node) ->
  {AffectedHashValueSet, NHashRule} =
    lists:foldl(
      fun(VirtualIndex, {TmpHashValueSet, TempHashRule}) ->
        VirtualNodeName = to_virtual_node(Node, VirtualIndex),
        Value = hash_string(VirtualNodeName),
        HashValue = Value rem ?MAX_INDEX,
        case gb_trees:lookup(HashValue, TempHashRule#hash_rule.node_tree) of
          none -> %%不存在虚拟节点
            NTempVirtualNodeTree = gb_trees:insert(HashValue, VirtualNodeName, TempHashRule#hash_rule.node_tree),
            case update_hash_value_list(HashValue, TempHashRule#hash_rule.rem_value_list) of
              {NTempHashValueList, -1} ->
                {TmpHashValueSet, TempHashRule#hash_rule{node_tree = NTempVirtualNodeTree, rem_value_list = NTempHashValueList}};
              {NTempHashValueList, NextHashValue} ->
                {gb_sets:add_element(NextHashValue, TmpHashValueSet), TempHashRule#hash_rule{node_tree = NTempVirtualNodeTree, rem_value_list = NTempHashValueList}}
            end;
          {value, _} ->
            {TmpHashValueSet, TempHashRule}
        end
      end, {gb_sets:new(), HashRule}, lists:seq(1, ?VIRTUAL_NODE_COUNT)),

  AffectedNodeSet =
    lists:foldr(
      fun(NextHashValue, TempNextNodeSet) ->
        VirtualNodeName = gb_trees:get(NextHashValue, NHashRule#hash_rule.node_tree),
        TmpNode = to_node(VirtualNodeName),
        case list_to_atom(Node) =:= TmpNode of
          true -> TempNextNodeSet;
          false -> gb_sets:add_element(TmpNode, TempNextNodeSet)
        end
      end, gb_sets:new(), gb_sets:to_list(AffectedHashValueSet)),
  {gb_sets:to_list(AffectedNodeSet), NHashRule}.

update_hash_rule(Node, HashRule, remove) when is_atom(Node) andalso is_record(HashRule, hash_rule) ->
  update_hash_rule(atom_to_list(Node), HashRule, remove);
update_hash_rule(Node, HashRule, remove) when is_list(Node) ->
  lists:foldr(
    fun(VirtualIndex, #hash_rule{node_tree = NodeTree, rem_value_list = HashValueList} = TempHashRule) ->
      VirtualNodeName = to_virtual_node(Node, VirtualIndex),
      Value = hash_string(VirtualNodeName),
      HashValue = Value rem ?MAX_INDEX,
      case gb_trees:is_defined(HashValue, NodeTree) of
        false -> TempHashRule;
        true ->
          TempHashRule#hash_rule{
            node_tree = gb_trees:delete(HashValue, NodeTree),
            rem_value_list = lists:delete(HashValue, HashValueList)
          }
      end
    end, HashRule, lists:seq(1, ?VIRTUAL_NODE_COUNT)).



%%更新hash值列表
update_hash_value_list(HashValue, []) -> {[HashValue], -1};
update_hash_value_list(HashValue, [Item]) ->
  if
    HashValue > Item -> {[Item, HashValue], Item};
    HashValue < Item -> {[HashValue, Item], Item}
  end;
update_hash_value_list(HashValue, ValueList) ->
  SortList = lists:sort([HashValue | ValueList]),
  NextValue = find_key_next(HashValue, SortList),
  {SortList, NextValue}.


%%找到key之后的元素，List为循环列表
find_key_next(Key, CycleList) ->
  Index = find_key_index(Key, CycleList, 1),
  Len = length(CycleList),
  if
    Index =:= Len -> lists:nth(1, CycleList);
    Index < Len -> lists:nth(Index + 1, CycleList)
  end.
find_key_index(_, [], Index) -> Index + 1;
find_key_index(Key, [H | T], Index) ->
  if
    Key =:= H -> Index;
    true -> find_key_index(Key, T, Index + 1)
  end.


%%根据hash key 查找存储节点
find_key_store_node(Key, HashRule) when is_list(Key) andalso is_record(HashRule, hash_rule) ->
  KeyHashValue = hash_string(Key) rem ?MAX_INDEX,
  case find_key_store_node_index(KeyHashValue, HashRule#hash_rule.rem_value_list) of
    {success, NodeHashValue} -> {success, to_node(gb_trees:get(NodeHashValue, HashRule#hash_rule.node_tree))};
    fail -> fail
  end.

find_key_store_node_index(_KeyHashVal, []) -> fail;
find_key_store_node_index(KeyHashValue, HashValueList) ->
  find_key_store_node_index(KeyHashValue, HashValueList, []).

find_key_store_node_index(_KeyHashValue, [], L) ->
  [NodeIndex | _] = lists:reverse(L),
  {success, NodeIndex};
find_key_store_node_index(KeyHashValue, [NodeIndexValue | T], L) when KeyHashValue > NodeIndexValue ->
  find_key_store_node_index(KeyHashValue, T, [NodeIndexValue | L]);
find_key_store_node_index(KeyHashValue, [NodeIndexValue | _], _L) when KeyHashValue =< NodeIndexValue ->
  {success, NodeIndexValue}.




