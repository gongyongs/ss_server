-module(rm_buff_data).
-export([get_data/1,get_indexs/0]).
get_data(0) -> {rm_buff_data,0,0,0,0,0,<<"爱谁谁">>,<<"card_satus">>,<<"随机发动一种其他状态卡的效果">>,0};
get_data(1) -> {rm_buff_data,1,1,0,0,0,<<"多点选择">>,<<"card_satus">>,<<"增加一个选择区域，战斗失败后消失。">>,0};
get_data(2) -> {rm_buff_data,2,1,0,0,0,<<"一张白纸">>,<<"card_satus">>,<<"状态卡的效果无效化。（白衣天使无视此效果）（DEBUFF）">>,1};
get_data(3) -> {rm_buff_data,3,1,1024,0,0,<<"白衣天使">>,<<"card_satus">>,<<"就算选择了有害状态的卡片也不会产生效果；取消已经发动的有害状态卡片的效果；不会再出现有害状态卡。">>,0};
get_data(4) -> {rm_buff_data,4,1,0,0,0,<<"喜欢挑战">>,<<"card_satus">>,<<"关卡难度＋1。（若无连胜场次，难度为连胜场次2,；若有连胜场次，视为连胜场次+1效果）">>,0};
get_data(5) -> {rm_buff_data,5,0,1,0,0,<<"低水平较量">>,<<"card_satus">>,<<"下次翻开的战斗卡片难度为“最低”。">>,0};
get_data(6) -> {rm_buff_data,6,0,2,0,0,<<"倒垃圾">>,<<"card_satus">>,<<"丢弃选择区域的卡片，并且下回合选择区域再度出现倒垃圾卡。">>,0};
get_data(7) -> {rm_buff_data,7,0,4,0,0,<<"中彩票">>,<<"card_satus">>,<<"下次翻开的卡片全变成宝箱卡和状态卡。">>,0};
get_data(8) -> {rm_buff_data,8,0,8,0,0,<<"老板去死">>,<<"card_satus">>,<<"丢弃选择区域中的ＢＯＳＳ战斗卡。">>,0};
get_data(9) -> {rm_buff_data,9,0,16,0,0,<<"口袋破了">>,<<"card_satus">>,<<"所持金币数量减半。（DEBUFF）">>,1};
get_data(10) -> {rm_buff_data,10,1,32,0,0,<<"土豪">>,<<"card_satus">>,<<"宝箱卡出现几率上升。（权值翻倍）">>,0};
get_data(11) -> {rm_buff_data,11,1,64,0,0,<<"状态来了">>,<<"card_satus">>,<<"状态卡出现几率上升。（权值翻倍）">>,0};
get_data(12) -> {rm_buff_data,12,1,128,0,0,<<"高水平较量">>,<<"card_satus">>,<<"精英战斗卡片出现几率上升。（权值翻倍）">>,0};
get_data(13) -> {rm_buff_data,13,1,256,0,0,<<"穷光蛋">>,<<"card_satus">>,<<"不会出现宝箱卡。（DEBUFF）">>,1};
get_data(14) -> {rm_buff_data,14,1,512,0,0,<<"不想刷新">>,<<"card_satus">>,<<"不会刷新未选择的卡片。">>,0};
get_data(15) -> {rm_buff_data,15,1,0,1,0,<<"霸气十足">>,<<"card_satus">>,<<"战斗后斗气完全回复。">>,0};
get_data(16) -> {rm_buff_data,16,1,0,2,0,<<"霸气漏光">>,<<"card_satus">>,<<"战斗胜利后也不会回复斗气。（霸气十足无效）（DEBUFF）">>,1};
get_data(17) -> {rm_buff_data,17,1,0,4,0,<<"守财奴">>,<<"card_satus">>,<<"战斗胜利后获得的赏金数量增加20%。">>,0};
get_data(18) -> {rm_buff_data,18,1,0,8,0,<<"机关枪">>,<<"card_satus">>,<<"每获得一场胜利，连胜场次+2。">>,0};
get_data(19) -> {rm_buff_data,19,1,0,0,1,<<"就是任性">>,<<"card_satus">>,<<"战斗失败也不会减少赏金和连胜场次。">>,0};


get_data(_Id) -> [].
get_indexs() -> [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19].
