-module(rm_difficulty).
-export([get_data/1,get_indexs/0]).
get_data(0) -> {rm_difficulty,0,<<"新手">>,<<"card_newbie">>,<<"101">>,10};
get_data(1) -> {rm_difficulty,1,<<"进阶">>,<<"card_advanced">>,<<"101">>,10};
get_data(2) -> {rm_difficulty,2,<<"精英">>,<<"card_elite">>,<<"102">>,10};
get_data(3) -> {rm_difficulty,3,<<"大师">>,<<"card_master">>,<<"102">>,10};


get_data(_Id) -> [].
get_indexs() -> [0,1,2,3].
