-module(defendDistanceInfo).
-export([get_data/1,get_indexs/0]).
get_data(1) -> {defendDistanceInfo,1,1.8};
get_data(2) -> {defendDistanceInfo,2,1.8};
get_data(3) -> {defendDistanceInfo,3,3};
get_data(4) -> {defendDistanceInfo,4,1.8};


get_data(_Id) -> [].
get_indexs() -> [1,2,3,4].
