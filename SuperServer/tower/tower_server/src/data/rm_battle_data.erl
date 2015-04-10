-module(rm_battle_data).
-export([get_data/1,get_indexs/0]).
get_data(1) -> {rm_battle_data,1,0,<<"monster">>,<<"card_CommonBoss_01">>,1006,1007,1008,100,10,<<"0">>,<<"小怪">>};
get_data(2) -> {rm_battle_data,2,1,<<"elite">>,<<"card_CommonBoss_02">>,1003,1004,1005,200,20,<<"0">>,<<"精英">>};
get_data(3) -> {rm_battle_data,3,2,<<"boss">>,<<"card_CommonBoss_03">>,1000,1001,1002,300,30,<<"0">>,<<"boss">>};
get_data(4) -> {rm_battle_data,4,3,<<"red">>,<<"card_RedBoss">>,1000,1001,1002,400,40,<<"0">>,<<"红卡">>};


get_data(_Id) -> [].
get_indexs() -> [1,2,3,4].
