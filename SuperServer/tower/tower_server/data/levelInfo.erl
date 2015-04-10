-module(levelInfo).
-export([get_data/1,get_indexs/0]).
get_data(100) -> {levelInfo,100,100,1000,1002,1004};
get_data(101) -> {levelInfo,101,101,1001,1003,1005};
get_data(102) -> {levelInfo,102,102,1006,1008,1004};
get_data(103) -> {levelInfo,103,103,1007,1009,1005};
get_data(104) -> {levelInfo,104,104,1000,1007,1004};
get_data(105) -> {levelInfo,105,105,1001,1006,1005};
get_data(106) -> {levelInfo,106,106,1000,1002,1008};
get_data(107) -> {levelInfo,107,107,1001,1003,1009};
get_data(108) -> {levelInfo,108,108,1006,1002,1008};
get_data(109) -> {levelInfo,109,109,1007,1003,1009};


get_data(_Id) -> [].
get_indexs() -> [100,101,102,103,104,105,106,107,108,109].
