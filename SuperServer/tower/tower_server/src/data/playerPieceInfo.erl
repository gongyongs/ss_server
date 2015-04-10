-module(playerPieceInfo).
-export([get_data/1,get_indexs/0]).
get_data(20000) -> {playerPieceInfo,20000,1000};
get_data(20001) -> {playerPieceInfo,20001,1001};
get_data(20002) -> {playerPieceInfo,20002,1002};
get_data(20003) -> {playerPieceInfo,20003,1003};
get_data(20004) -> {playerPieceInfo,20004,1004};
get_data(20005) -> {playerPieceInfo,20005,1005};
get_data(20006) -> {playerPieceInfo,20006,1006};
get_data(20007) -> {playerPieceInfo,20007,1007};
get_data(20008) -> {playerPieceInfo,20008,1008};
get_data(20009) -> {playerPieceInfo,20009,1009};


get_data(_Id) -> [].
get_indexs() -> [20000,20001,20002,20003,20004,20005,20006,20007,20008,20009].
