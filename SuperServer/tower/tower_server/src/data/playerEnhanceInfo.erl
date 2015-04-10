-module(playerEnhanceInfo).
-export([get_data/1,get_indexs/0]).
get_data(1) -> {playerEnhanceInfo,1,5};
get_data(2) -> {playerEnhanceInfo,2,20};
get_data(3) -> {playerEnhanceInfo,3,100};
get_data(4) -> {playerEnhanceInfo,4,200};
get_data(5) -> {playerEnhanceInfo,5,500};


get_data(_Id) -> [].
get_indexs() -> [1,2,3,4,5].
