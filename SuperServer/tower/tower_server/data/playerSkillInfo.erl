-module(playerSkillInfo).
-export([get_data/1,get_indexs/0]).
get_data(1000) -> {playerSkillInfo,1000,1000,2008,1,4033,0,0,0,0,0,0,0};
get_data(1001) -> {playerSkillInfo,1001,1001,2011,1,0,0,0,0,0,1,0,0};
get_data(1007) -> {playerSkillInfo,1007,1007,2003,1,4003,0,0,0,0,1,0,0};
get_data(1003) -> {playerSkillInfo,1003,1003,2000,1,4015,0,0,0,0,1,0,0};
get_data(1004) -> {playerSkillInfo,1004,1004,2004,1,4030,1,0,0,0,1,0,0};
get_data(1005) -> {playerSkillInfo,1005,1005,2009,1,4028,0,0,0,0,1,0,0};
get_data(1006) -> {playerSkillInfo,1006,1006,2002,1,4035,0,0,0,0,1,0,0};
get_data(1008) -> {playerSkillInfo,1008,1008,2010,1,4032,0,0,0,0,1,0,0};
get_data(1002) -> {playerSkillInfo,1002,1002,2012,1,4034,0,0,0,0,1,0,0};
get_data(1009) -> {playerSkillInfo,1009,1009,2001,1,4031,0,0,0,0,1,0,0};


get_data(_Id) -> [].
get_indexs() -> [1000,1001,1007,1003,1004,1005,1006,1008,1002,1009].
