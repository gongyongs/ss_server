-module(soundTriggerInfo).
-export([get_data/1,get_indexs/0]).
get_data({1,0}) -> {soundTriggerInfo,1,1,<<"">>,2,<<"voice/00108">>};
get_data({2,0}) -> {soundTriggerInfo,2,2,<<"">>,2,<<"voice/00107">>};
get_data({3,0}) -> {soundTriggerInfo,3,3,<<"">>,2,<<"voice/00114">>};
get_data({3,1}) -> {soundTriggerInfo,3,3,<<"">>,2,<<"voice/Block">>};
get_data({4,0}) -> {soundTriggerInfo,4,4,<<"TouLan_GouShou">>,2,<<"voice/Hookshot">>};
get_data({4,1}) -> {soundTriggerInfo,4,4,<<"TouLan_LanXiaTouLan">>,2,<<"voice/00209">>};
get_data({4,2}) -> {soundTriggerInfo,4,4,<<"">>,2,<<"voice/Jumpshot">>};
get_data({4,3}) -> {soundTriggerInfo,4,4,<<"TouLan_HouYangTiaoTou">>,2,<<"voice/Fadeaway">>};
get_data({5,0}) -> {soundTriggerInfo,5,5,<<"TouLan_GouShou">>,2,<<"voice/Hookshot">>};
get_data({5,1}) -> {soundTriggerInfo,5,5,<<"TouLan_LanXiaTouLan">>,2,<<"voice/00209">>};
get_data({5,2}) -> {soundTriggerInfo,5,5,<<"">>,2,<<"voice/00189">>};
get_data({5,3}) -> {soundTriggerInfo,5,5,<<"TouLan_HouYangTiaoTou">>,2,<<"voice/Fadeaway">>};
get_data({6,0}) -> {soundTriggerInfo,6,6,<<"">>,2,<<"voice/00191">>};
get_data({6,1}) -> {soundTriggerInfo,6,6,<<"">>,2,<<"voice/00152">>};
get_data({6,2}) -> {soundTriggerInfo,6,6,<<"">>,2,<<"voice/Niceshot">>};
get_data({7,0}) -> {soundTriggerInfo,7,7,<<"">>,2,<<"voice/00206">>};
get_data({8,0}) -> {soundTriggerInfo,8,8,<<"">>,2,<<"voice/00150">>};
get_data({8,1}) -> {soundTriggerInfo,8,8,<<"">>,2,<<"voice/00154">>};
get_data({8,2}) -> {soundTriggerInfo,8,8,<<"">>,2,<<"voice/Rebound">>};
get_data({9,0}) -> {soundTriggerInfo,9,9,<<"">>,2,<<"voice/00157">>};
get_data({9,1}) -> {soundTriggerInfo,9,9,<<"">>,2,<<"voice/00160">>};
get_data({9,2}) -> {soundTriggerInfo,9,9,<<"">>,2,<<"voice/Dribble">>};
get_data({10,0}) -> {soundTriggerInfo,10,10,<<"">>,2,<<"voice/00166">>};
get_data({10,1}) -> {soundTriggerInfo,10,10,<<"ShangLan_LaGanShangLan">>,2,<<"voice/00207">>};
get_data({10,2}) -> {soundTriggerInfo,10,10,<<"ShangLan_LingQiaoShangLan">>,2,<<"voice/00167">>};
get_data({10,3}) -> {soundTriggerInfo,10,10,<<"ShangLan_LingQiaoShangLan_DiXian">>,2,<<"voice/00167">>};
get_data({11,0}) -> {soundTriggerInfo,11,11,<<"">>,2,<<"voice/Watch Out">>};
get_data({11,1}) -> {soundTriggerInfo,11,11,<<"KouLan_DaFengChe">>,2,<<"voice/00170">>};
get_data({12,0}) -> {soundTriggerInfo,12,12,<<"">>,2,<<"voice/Steal">>};
get_data({13,0}) -> {soundTriggerInfo,13,13,<<"">>,2,<<"voice/Steal">>};
get_data({16,0}) -> {soundTriggerInfo,16,16,<<"">>,2,<<"voice/00173">>};
get_data({14,0}) -> {soundTriggerInfo,14,14,<<"">>,2,<<"voice/Ally You">>};
get_data({15,0}) -> {soundTriggerInfo,15,15,<<"">>,2,<<"voice/You You">>};
get_data({17,0}) -> {soundTriggerInfo,17,17,<<"">>,1,<<"se/00389">>};
get_data({18,0}) -> {soundTriggerInfo,18,18,<<"">>,2,<<"voice/00211">>};
get_data({19,0}) -> {soundTriggerInfo,19,19,<<"">>,2,<<"voice/00186">>};


get_data(_Id) -> [].
get_indexs() -> [{1,0},{2,0},{3,0},{3,1},{4,0},{4,1},{4,2},{4,3},{5,0},{5,1},{5,2},{5,3},{6,0},{6,1},{6,2},{7,0},{8,0},{8,1},{8,2},{9,0},{9,1},{9,2},{10,0},{10,1},{10,2},{10,3},{11,0},{11,1},{12,0},{13,0},{16,0},{14,0},{15,0},{17,0},{18,0},{19,0}].
