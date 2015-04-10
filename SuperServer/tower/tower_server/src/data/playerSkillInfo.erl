-module(playerSkillInfo).
-export([get_data/1,get_indexs/0]).
get_data(1000) -> {playerSkillInfo,1000,10009,1,4001,0,0,0,0,0,0,0};
get_data(1001) -> {playerSkillInfo,1001,10006,1,4009,0,0,0,0,1,0,0};
get_data(1007) -> {playerSkillInfo,1007,10007,1,4004,0,0,0,0,1,0,0};
get_data(1003) -> {playerSkillInfo,1003,10000,1,4015,0,0,0,0,1,0,0};
get_data(1004) -> {playerSkillInfo,1004,10005,1,4030,1,0,0,0,1,0,0};
get_data(1005) -> {playerSkillInfo,1005,10010,1,4028,0,0,0,0,1,0,0};
get_data(1006) -> {playerSkillInfo,1006,10004,1,4035,0,0,0,0,1,0,0};
get_data(1008) -> {playerSkillInfo,1008,10008,1,4032,0,0,0,0,1,0,0};
get_data(1002) -> {playerSkillInfo,1002,10001,1,4034,0,0,0,0,1,0,0};
get_data(1009) -> {playerSkillInfo,1009,10002,1,4031,0,0,0,0,1,0,0};
get_data(1010) -> {playerSkillInfo,1010,10007,1,4035,0,0,0,0,1,0,0};
get_data(1011) -> {playerSkillInfo,1011,10002,1,4015,0,0,0,0,1,0,0};
get_data(1012) -> {playerSkillInfo,1012,10002,1,4015,0,0,0,0,1,0,0};
get_data(1013) -> {playerSkillInfo,1013,10002,1,4015,0,0,0,0,1,0,0};
get_data(1014) -> {playerSkillInfo,1014,10000,1,4015,0,0,0,0,1,0,0};


get_data(_Id) -> [].
get_indexs() -> [1000,1001,1007,1003,1004,1005,1006,1008,1002,1009,1010,1011,1012,1013,1014].
