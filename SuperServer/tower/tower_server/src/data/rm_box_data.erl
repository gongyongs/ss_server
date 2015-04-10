-module(rm_box_data).
-export([get_data/1,get_indexs/0]).
get_data(0) -> {rm_box_data,0,0,<<"宝箱">>,<<"card_chests">>,100};


get_data(_Id) -> [].
get_indexs() -> [0].
