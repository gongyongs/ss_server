-module(equipMaterialInfo).
-export([get_data/1,get_indexs/0]).
get_data(1000) -> {equipMaterialInfo,1000,1000,<<"新奥尔良烤翅">>,<<"热乎乎刚出炉的鸡翅">>,<<"新手级赏金赛">>,100};
get_data(1001) -> {equipMaterialInfo,1001,1001,<<"丹佛">>,<<"好大一块金子啊">>,<<"新手级赏金赛">>,100};
get_data(1002) -> {equipMaterialInfo,1002,1002,<<"奥兰多">>,<<"这是一根神奇的魔术棒">>,<<"新手级赏金赛">>,100};
get_data(1003) -> {equipMaterialInfo,1003,1003,<<"印第安纳">>,<<"元素之力会毁灭你">>,<<"新手级赏金赛">>,100};
get_data(1004) -> {equipMaterialInfo,1004,1004,<<"犹他">>,<<"盐湖城里有好多盐">>,<<"新手级赏金赛">>,100};
get_data(1005) -> {equipMaterialInfo,1005,1005,<<"克利夫兰">>,<<"我有一把骑士剑">>,<<"进阶级赏金赛">>,100};
get_data(1006) -> {equipMaterialInfo,1006,1006,<<"亚特兰大">>,<<"老鹰抓小鸡">>,<<"进阶级赏金赛">>,100};
get_data(1007) -> {equipMaterialInfo,1007,1007,<<"菲尼克斯">>,<<"凤凰可不是鸡变的">>,<<"进阶级赏金赛">>,100};
get_data(1008) -> {equipMaterialInfo,1008,1008,<<"夏洛特">>,<<"我只知道魔兽里的影子山猫">>,<<"进阶级赏金赛">>,100};
get_data(1009) -> {equipMaterialInfo,1009,1009,<<"多伦多">>,<<"恐龙时代来临了">>,<<"进阶级赏金赛">>,100};
get_data(1010) -> {equipMaterialInfo,1010,1010,<<"雷霆">>,<<"雷神之锤">>,<<"精英级赏金赛">>,100};
get_data(1011) -> {equipMaterialInfo,1011,1011,<<"达拉斯">>,<<"不用麻烦了，牛仔很忙的">>,<<"精英级赏金赛">>,100};
get_data(1012) -> {equipMaterialInfo,1012,1012,<<"纽约">>,<<"纽约人民欢迎你">>,<<"精英级赏金赛">>,100};
get_data(1013) -> {equipMaterialInfo,1013,1013,<<"休斯顿">>,<<"航天城里有姚明">>,<<"精英级赏金赛">>,100};
get_data(1014) -> {equipMaterialInfo,1014,1014,<<"底特律">>,<<"汽车城已经破产了">>,<<"精英级赏金赛">>,100};
get_data(1015) -> {equipMaterialInfo,1015,1015,<<"迈阿密">>,<<"为什么这里这么热">>,<<"大师级赏金赛">>,100};
get_data(1016) -> {equipMaterialInfo,1016,1016,<<"圣安东尼奥">>,<<"马刺这种东西从没见过">>,<<"大师级赏金赛">>,100};
get_data(1017) -> {equipMaterialInfo,1017,1017,<<"芝加哥">>,<<"一只公牛没有角">>,<<"大师级赏金赛">>,100};
get_data(1018) -> {equipMaterialInfo,1018,1018,<<"洛杉鸡">>,<<"洛杉矶是一只巨大的鸡">>,<<"大师级赏金赛">>,100};
get_data(1019) -> {equipMaterialInfo,1019,1019,<<"波士顿">>,<<"这盾牌能抗住BOSS么">>,<<"大师级赏金赛">>,100};


get_data(_Id) -> [].
get_indexs() -> [1000,1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014,1015,1016,1017,1018,1019].
