-module(sceneInfo).
-export([get_data/1,get_indexs/0]).
get_data(100) -> {sceneInfo,100,<<"老旧工厂">>,<<"scene_ny">>};
get_data(101) -> {sceneInfo,101,<<"迪拜沙滩">>,<<"scene_dubai">>};
get_data(102) -> {sceneInfo,102,<<"米高梅酒店">>,<<"scene_mgm">>};
get_data(103) -> {sceneInfo,103,<<"北京故宫">>,<<"scene_bj">>};


get_data(_Id) -> [].
get_indexs() -> [100,101,102,103].
