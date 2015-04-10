-module(sequenceSetInfo).
-export([get_data/1,get_indexs/0]).
get_data(1000) -> {sequenceSetInfo,1000,1000,1000,1001,1002,1003,1004,0,0,0,0,0};
get_data(1001) -> {sequenceSetInfo,1001,1001,1006,1007,1008,1009,0,0,0,0,0,0};
get_data(1002) -> {sequenceSetInfo,1002,1002,1013,0,0,0,0,0,0,0,0,0};
get_data(1003) -> {sequenceSetInfo,1003,1003,1017,0,0,0,0,0,0,0,0,0};
get_data(1004) -> {sequenceSetInfo,1004,1004,1020,0,0,0,0,0,0,0,0,0};
get_data(1005) -> {sequenceSetInfo,1005,1005,0,0,0,0,0,0,0,0,0,0};
get_data(1006) -> {sequenceSetInfo,1006,1006,1026,0,0,0,0,0,0,0,0,0};
get_data(1007) -> {sequenceSetInfo,1007,1007,1029,1030,0,0,0,0,0,0,0,0};
get_data(2001) -> {sequenceSetInfo,2001,2001,1010,1011,0,0,0,0,0,0,0,0};
get_data(2006) -> {sequenceSetInfo,2006,2006,1028,0,0,0,0,0,0,0,0,0};
get_data(2004) -> {sequenceSetInfo,2004,2004,1021,0,0,0,0,0,0,0,0,0};
get_data(2003) -> {sequenceSetInfo,2003,2003,1018,0,0,0,0,0,0,0,0,0};
get_data(2002) -> {sequenceSetInfo,2002,2002,1015,0,0,0,0,0,0,0,0,0};
get_data(2000) -> {sequenceSetInfo,2000,2000,1005,0,0,0,0,0,0,0,0,0};
get_data(2005) -> {sequenceSetInfo,2005,2005,1025,0,0,0,0,0,0,0,0,0};
get_data(2007) -> {sequenceSetInfo,2007,2007,1031,0,0,0,0,0,0,0,0,0};
get_data(3007) -> {sequenceSetInfo,3007,3007,1029,1030,1031,0,0,0,0,0,0,0};
get_data(1008) -> {sequenceSetInfo,1008,1008,1026,0,0,0,0,0,0,0,0,0};
get_data(2008) -> {sequenceSetInfo,2008,2008,1032,1033,0,0,0,0,0,0,0,0};
get_data(3008) -> {sequenceSetInfo,3008,3008,1026,1032,1033,0,0,0,0,0,0,0};
get_data(1009) -> {sequenceSetInfo,1009,1009,1034,0,0,0,0,0,0,0,0,0};
get_data(2009) -> {sequenceSetInfo,2009,2009,1035,0,0,0,0,0,0,0,0,0};
get_data(3009) -> {sequenceSetInfo,3009,3009,1034,1035,0,0,0,0,0,0,0,0};
get_data(1010) -> {sequenceSetInfo,1010,1010,1036,0,0,0,0,0,0,0,0,0};
get_data(2010) -> {sequenceSetInfo,2010,2010,1037,1038,0,0,0,0,0,0,0,0};
get_data(3010) -> {sequenceSetInfo,3010,3010,1036,1037,1038,0,0,0,0,0,0,0};
get_data(1011) -> {sequenceSetInfo,1011,1011,1039,0,0,0,0,0,0,0,0,0};
get_data(2011) -> {sequenceSetInfo,2011,2011,1038,0,0,0,0,0,0,0,0,0};


get_data(_Id) -> [].
get_indexs() -> [1000,1001,1002,1003,1004,1005,1006,1007,2001,2006,2004,2003,2002,2000,2005,2007,3007,1008,2008,3008,1009,2009,3009,1010,2010,3010,1011,2011].
