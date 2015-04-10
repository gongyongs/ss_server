-module(playerModelInfo).
-export([get_data/1,get_indexs/0]).
get_data(1000) -> {playerModelInfo,1000,<<"male">>,<<"male_ZhangBoLun">>,<<"male_ZhangBoLun">>};
get_data(1001) -> {playerModelInfo,1001,<<"male">>,<<"male_MuTuoMuBo">>,<<"male_MuTuoMuBo">>};
get_data(1002) -> {playerModelInfo,1002,<<"male">>,<<"male_BoDe">>,<<"male_BoDe">>};
get_data(1003) -> {playerModelInfo,1003,<<"male">>,<<"male_AnDongNi">>,<<"male_AnDongNi">>};
get_data(1004) -> {playerModelInfo,1004,<<"male">>,<<"male_TuoMaSi">>,<<"male_TuoMaSi">>};
get_data(1005) -> {playerModelInfo,1005,<<"female">>,<<"female_01_n">>,<<"female_PeiDun_dif">>};
get_data(1006) -> {playerModelInfo,1006,<<"male">>,<<"male_GeLiFen">>,<<"male_GeLiFen">>};
get_data(1007) -> {playerModelInfo,1007,<<"male">>,<<"male_DengKen">>,<<"male_DengKen">>};
get_data(1008) -> {playerModelInfo,1008,<<"male">>,<<"male_QiaoZhiGeWen">>,<<"male_QiaoZhiGeWen">>};
get_data(1009) -> {playerModelInfo,1009,<<"male">>,<<"male_LeiALun">>,<<"male_LeiALun">>};
get_data(1010) -> {playerModelInfo,1010,<<"male">>,<<"male_ZiYuan">>,<<"male_ZiYuan">>};
get_data(1011) -> {playerModelInfo,1011,<<"male">>,<<"male_QingFeng">>,<<"male_QingFeng">>};
get_data(1012) -> {playerModelInfo,1012,<<"male">>,<<"male_HuangLai">>,<<"male_HuangLai">>};
get_data(1013) -> {playerModelInfo,1013,<<"male">>,<<"male_LvJian">>,<<"male_LvJian">>};
get_data(1014) -> {playerModelInfo,1014,<<"male">>,<<"male_ChiSi">>,<<"male_ChiSi">>};


get_data(_Id) -> [].
get_indexs() -> [1000,1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014].
