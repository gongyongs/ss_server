-module(sceneInfo).
-export([get_data/1,get_indexs/0]).
get_data(100) -> {sceneInfo,100,100,<<"老旧工厂">>,<<"scene_ny">>};
get_data(101) -> {sceneInfo,101,101,<<"迪拜沙滩">>,<<"scene_dubai">>};
get_data(102) -> {sceneInfo,102,102,<<"米高梅酒店">>,<<"scene_mgm">>};


get_data(_Id) -> [].
get_indexs() -> [100,101,102].
