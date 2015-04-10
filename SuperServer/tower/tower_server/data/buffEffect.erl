-module(buffEffect).
-export([get_data/1,get_indexs/0]).
get_data({1000,0}) -> {buffEffect,1000,1000,114,0,2,0,0};
get_data({1000,1}) -> {buffEffect,1000,1000,117,10,0,0,0};
get_data({1001,0}) -> {buffEffect,1001,1001,108,0,2,0,0};
get_data({1001,1}) -> {buffEffect,1001,1001,109,0,2,0,0};
get_data({1001,2}) -> {buffEffect,1001,1001,110,0,2,0,0};
get_data({1001,3}) -> {buffEffect,1001,1001,114,0,2,0,0};
get_data({1002,0}) -> {buffEffect,1002,1002,302,0,0,0,0};
get_data({1003,0}) -> {buffEffect,1003,1003,108,0,2,0,0};
get_data({1003,1}) -> {buffEffect,1003,1003,114,0,2,0,0};
get_data({1004,0}) -> {buffEffect,1004,1004,116,0,1.1,0,0};
get_data({1005,0}) -> {buffEffect,1005,1005,310,211,0,0,0};
get_data({1005,1}) -> {buffEffect,1005,1005,310,212,0,0,0};
get_data({1005,2}) -> {buffEffect,1005,1005,310,213,0,0,0};
get_data({1005,3}) -> {buffEffect,1005,1005,310,215,0,0,0};
get_data({1006,0}) -> {buffEffect,1006,1006,111,0,2,0,0};
get_data({1006,1}) -> {buffEffect,1006,1006,112,0,2,0,0};
get_data({1006,2}) -> {buffEffect,1006,1006,113,0,2,0,0};
get_data({1006,3}) -> {buffEffect,1006,1006,115,0,2,0,0};
get_data({1007,0}) -> {buffEffect,1007,1007,108,0,1.5,0,0};
get_data({1007,1}) -> {buffEffect,1007,1007,109,0,1.5,0,0};
get_data({1007,2}) -> {buffEffect,1007,1007,110,0,1.5,0,0};
get_data({1007,3}) -> {buffEffect,1007,1007,114,0,1.5,0,0};
get_data({1008,0}) -> {buffEffect,1008,1008,108,0,1.3,0,0};
get_data({1008,1}) -> {buffEffect,1008,1008,109,0,1.3,0,0};
get_data({1008,2}) -> {buffEffect,1008,1008,110,0,1.3,0,0};
get_data({1008,3}) -> {buffEffect,1008,1008,114,0,1.3,0,0};
get_data({1009,0}) -> {buffEffect,1009,1009,116,0,2,0,0};
get_data({1010,0}) -> {buffEffect,1010,1010,119,3,1,0,0};
get_data({1011,0}) -> {buffEffect,1011,1011,116,0,1.2,0,0};
get_data({1012,0}) -> {buffEffect,1012,1012,116,0,1.3,0,0};
get_data({1013,0}) -> {buffEffect,1013,1013,109,0,2,0,0};
get_data({1013,1}) -> {buffEffect,1013,1013,110,0,2,0,0};
get_data({1013,2}) -> {buffEffect,1013,1013,122,50000,0,0,0};
get_data({1014,0}) -> {buffEffect,1014,1014,109,0,2,0,0};
get_data({1014,1}) -> {buffEffect,1014,1014,110,0,2,0,0};
get_data({1015,0}) -> {buffEffect,1015,1015,109,0,2,0,0};
get_data({1015,1}) -> {buffEffect,1015,1015,110,0,2,0,0};
get_data({1016,0}) -> {buffEffect,1016,1016,105,0,1.3,0,0};
get_data({1017,0}) -> {buffEffect,1017,1017,111,0,1.5,0,0};
get_data({1017,1}) -> {buffEffect,1017,1017,112,0,1.5,0,0};
get_data({1017,2}) -> {buffEffect,1017,1017,113,0,1.5,0,0};
get_data({1017,3}) -> {buffEffect,1017,1017,115,0,1.5,0,0};
get_data({1018,0}) -> {buffEffect,1018,1018,302,0,0,0,0};
get_data({1019,0}) -> {buffEffect,1019,1019,116,0,1.5,0,0};
get_data({1020,0}) -> {buffEffect,1020,1020,123,0,2,0,0};
get_data({1021,0}) -> {buffEffect,1021,1021,116,0,1.5,0,0};
get_data({1022,0}) -> {buffEffect,1022,1022,302,0,0,0,0};
get_data({2000,0}) -> {buffEffect,2000,2000,222,100000,0,0,0};
get_data({2000,1}) -> {buffEffect,2000,2000,300,0,0,0,0};
get_data({2001,0}) -> {buffEffect,2001,2001,216,0,0.8,0,0};
get_data({2002,0}) -> {buffEffect,2002,2002,216,0,0.9,0,0};
get_data({2003,0}) -> {buffEffect,2003,2003,219,3,1,0,0};
get_data({2004,0}) -> {buffEffect,2004,2004,222,100000,0,0,0};
get_data({2004,1}) -> {buffEffect,2004,2004,300,0,0,0,0};
get_data({2005,0}) -> {buffEffect,2005,2005,222,100000,0,0,0};
get_data({2005,1}) -> {buffEffect,2005,2005,300,0,0,0,0};
get_data({2006,0}) -> {buffEffect,2006,2006,301,0,0,0,0};
get_data({2007,0}) -> {buffEffect,2007,2007,301,0,0,0,0};
get_data({2008,0}) -> {buffEffect,2008,2008,219,10,1,0,0};
get_data({2009,0}) -> {buffEffect,2009,2009,310,119,0,0,0};
get_data({2009,1}) -> {buffEffect,2009,2009,216,0,0.25,0,0};
get_data({2009,2}) -> {buffEffect,2009,2009,220,0,0.25,0,0};
get_data({2010,0}) -> {buffEffect,2010,2010,220,0,0.25,0,0};
get_data({2011,0}) -> {buffEffect,2011,2011,216,0,0.5,0,0};
get_data({2011,1}) -> {buffEffect,2011,2011,220,0,0.5,0,0};


get_data(_Id) -> [].
get_indexs() -> [{1000,0},{1000,1},{1001,0},{1001,1},{1001,2},{1001,3},{1002,0},{1003,0},{1003,1},{1004,0},{1005,0},{1005,1},{1005,2},{1005,3},{1006,0},{1006,1},{1006,2},{1006,3},{1007,0},{1007,1},{1007,2},{1007,3},{1008,0},{1008,1},{1008,2},{1008,3},{1009,0},{1010,0},{1011,0},{1012,0},{1013,0},{1013,1},{1013,2},{1014,0},{1014,1},{1015,0},{1015,1},{1016,0},{1017,0},{1017,1},{1017,2},{1017,3},{1018,0},{1019,0},{1020,0},{1021,0},{1022,0},{2000,0},{2000,1},{2001,0},{2002,0},{2003,0},{2004,0},{2004,1},{2005,0},{2005,1},{2006,0},{2007,0},{2008,0},{2009,0},{2009,1},{2009,2},{2010,0},{2011,0},{2011,1}].
