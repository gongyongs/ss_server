-module(playerActionAIInfo).
-export([get_data/1,get_indexs/0]).
get_data(1000) -> {playerActionAIInfo,1000,1000,<<"郑容和">>,100,5,0,0,0,0,4,2,5,0,10,0,0,0,5,10,0,0,0,5,0,0,0,5,5,0};
get_data(1001) -> {playerActionAIInfo,1001,1001,<<"马里奥">>,20,5,0,0,0,0,4,2,0,0,10,0,0,0,5,10,0,0,0,5,0,0,0,5,5,0};
get_data(1002) -> {playerActionAIInfo,1002,1002,<<"酋长">>,60,3,2,0,0,0,2,3,0,0,0,0,4,3,1,0,0,3,5,1,0,0,0,5,2,0};
get_data(1003) -> {playerActionAIInfo,1003,1003,<<"鹿晗">>,80,3,2,0,0,0,2,3,0,0,0,0,4,3,1,0,0,3,5,1,0,0,0,5,2,0};
get_data(1004) -> {playerActionAIInfo,1004,1004,<<"艾吉奥">>,20,1,4,0,0,0,0,4,2,1,0,0,3,3,2,0,0,5,3,3,0,0,3,3,4,0};
get_data(1005) -> {playerActionAIInfo,1005,1005,<<"毒药">>,20,1,4,0,0,0,0,4,2,1,0,0,3,3,2,0,0,5,3,3,0,0,3,3,4,0};
get_data(1006) -> {playerActionAIInfo,1006,1006,<<"古烈">>,80,5,0,0,0,0,4,2,0,3,8,0,3,3,3,8,0,0,0,5,0,0,0,0,5,0};
get_data(1007) -> {playerActionAIInfo,1007,1007,<<"少林">>,60,4,1,0,0,0,3,3,3,3,5,0,3,3,3,3,0,0,0,5,0,0,0,0,5,0};
get_data(1008) -> {playerActionAIInfo,1008,1008,<<"冰人">>,90,2,3,0,0,0,0,4,2,0,0,0,8,3,2,0,0,8,3,3,0,0,5,3,4,0};
get_data(1009) -> {playerActionAIInfo,1009,1009,<<"福尔摩斯">>,70,2,3,0,0,0,0,4,2,0,0,0,8,3,2,0,0,8,3,3,0,0,5,3,4,0};


get_data(_Id) -> [].
get_indexs() -> [1000,1001,1002,1003,1004,1005,1006,1007,1008,1009].
