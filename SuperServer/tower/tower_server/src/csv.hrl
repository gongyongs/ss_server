%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 七月 2014 上午10:32
%%%-------------------------------------------------------------------
-author("zqlt").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%SuperStar配置表record结构定义%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TYPE_COMPOSE_MATERIAL,1).		%%合成材料
-define(TYPE_PLAYER_PIECE,2).			%%球员碎片类型
-define(TYPE_EQUIP,3). 					%%装备类型
-define(TYPE_CONSUME_MATERIAL,4).		%%消耗品类型

-record(playerPieceInfo,{
	id = 0,					%%碎片id
	correspondingPlayer = 0 %%对应球员id
}).


-record(itemBaseInfo,{
	id = 0,					%%物品id
	itemIcon = <<"">>,		%%物品图标
	itemType  = 0,			%%物品类型
	name = <<"">>,			%%物品名称
	description = <<"">>,	%%描述
	getWay = <<"">>,		%%获取途径
	getWayIcon = <<"">>,	%%获取途径的图标
	price = 0				%%售价
}).

-record(rm_battle_data,{
	id = 0,					%%id
	type = 0,				%%类型
	name  = <<"">>,			%%名字
	icon = <<"">>,			%%图标
	playerID_1 = 0,			%%球员id1
	playerID_2 = 0,			%%球员id2
	playerID_3 = 0,			%%球员id3
	fee = 0,				%%胜利奖励
	feelose = 0,			%%失败扣除
	droplist = <<"">>,		%%掉落列表
	description = <<"">>	%%描述
}).

-record(rm_buff_data,{
	id = 0,					%%id
	type = 0,				%%类型
	flagRefresh = 0,		%%刷新标签
	flagWin = 0,			%%胜利标签
	flagLose = 0,			%%失败标签
	name  = <<"">>,			%%名字
	icon = <<"">>,			%%图标
	description = <<"">>,	%%描述
	isdebuff = 0			%%是否是debuff
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%华丽的分隔线%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -record(monster_group, {
%%   group_id::string(),
%%   monster_id_1::string(),
%%   monster_id_2::string(),
%%   monster_id_3::string()
%% }).
%%
%% -record(monster,{
%%   id::string(),
%%   type::string(),
%%   drop::string()
%% }).

-record(res_drop_config,{
  id::string(),                     %%掉落物品的id
  type::integer()                   %%掉落物品的类型：1.装备，2：铭文，3.材料，4.道具, 5装备进阶碎片，6 铭文进阶碎片
}).

-record(res_equipment,{
  id::string(),                                 %%装备id
  name::string(),                               %%装备名称
  dis_name::string(),                           %%装备显示名
  show_pic::string(),                           %%装备显示图片资源
  bullet_effect_path::string(),
  bullet_effect_name::string(),
  weapon_effect_path::string(),
  weapon_effect_name::string(),
  equip_effect_path::string(),
  equip_effect::string(),
  bullet_speed::integer(),
  can_attack::integer(),
  sound_event::string(),
  type::integer(),                              %%装备类型：1:主武器， 2：副武器， 3：头盔或者帽子
  max_level::integer(),                         %%最高等级
  star_level::integer(),                        %%星级
  belong_tower_type::integer(),                 %%所属职业：1:花生剑士:
  upgrade_obj_id1::string(),                    %%进阶路线1
  upgrade_obj_id2::string(),                    %%进阶路线2
  init_hp::integer(),                           %%生命值
  hp_per_level::integer(),                      %%每级生命增加值
  physical_attack_min::float(),                 %%物理基础攻击下限
  physical_attack_min_per_level::float(),       %%物理基础攻击下限每等级增加
  physical_attack_max::float(),                 %%物理基础攻击上限
  physical_attack_max_per_level::float(),       %%物理基础攻击上限每等级增加
  magic_attack_min::float(),                    %%魔法基础攻击下限
  magic_attack_min_per_level::float(),          %%魔法基础攻击下限每等级增加
  magic_attack_max::float(),                    %%魔法基础攻击上限
  magic_attack_max_per_level::float(),          %%魔法基础攻击上限每等级增加
  crit_attack_level::integer(),
  crit_attack_grow::float(),
  hit_level::integer(),
  hit_grow::float(),
  speed_level::integer(),
  speed_grow::float(),
  skill_id::string(),                           %%附带技能id
  price::integer(),                             %%售价
  gain_exp::integer(),                          %%被吞噬可获得经验
  gain_exp_per_level::integer(),                %%被吞噬可获得经验每等级增加
  base_dsm::integer(),                          %%基础战斗力
  dsm_per_level::integer(),                     %%战斗力每等级增加
  max_level_1::integer(),                       %%最大等级:（备份使用）
  base_score_addition::integer(),               %%基础分数加成
  base_score_addition_growth::float(),          %%基础分数加成成长
  icon::string(),                               %%图标
  advance_cost::integer(),                      %%进阶所消耗金币数量
  advance_material_1_id::string(),              %%进阶所需材料1的id
  advance_material_1_num::integer(),            %%进阶所需材料1的数量
  advance_material_2_id::string(),              %%进阶所需材料2的id
  advance_material_2_num::integer(),            %%进阶所需材料2的数量
  advance_material_3_id::string(),              %%进阶所需材料3的id
  advance_material_3_num::integer(),            %%进阶所需材料3的数量
  advance_material_4_id::string(),              %%进阶所需材料4的id
  advance_material_4_num::integer(),            %%进阶所需材料4的数量
  advance_material_5_id::string(),              %%进阶所需材料5的id
  advance_material_5_num::integer()            %%进阶所需材料5的数量
}).

-record(res_tower,{
  id::string(),                                 %%角色id
  name::string(),                               %%名称
  level::integer(),                             %%等级
  type::integer(),                              %%类型
  base_weapon_id::string(),                     %%基础主武器id
  base_second_weapon_id::string(),              %%基础副武器id
  base_hat_id::integer()                      %%基础头盔id
}).

%%材料
-record(res_material, {
  id::string(),             %%材料id
  name::string(),
  src::string(),
  star_level::integer(),    %%星级
  price::integer(),         %%售价
  gain_exp::integer(),      %%被吞噬后可获得经验值
  evolv_id::string(),       %%可以合成的材料
  evolv_count::integer(),   %%合成材料所需数量
  desc::string()            %%详细描述
}).


%%初始配置
-record(res_init_account, {
  id::integer(),
  init_gold::integer(),                       %%初始金币数
  init_gem::integer(),                        %%初始钻石
  init_strength::integer(),                   %%初始体力上限
  init_backpack_capacity::integer(),          %%初始背包容量
  init_heros::list(),
  init_property::list(),      %%初始道具
  init_material::list(),       %%初始材料
  init_select_hero::list()    %%初始选中的塔
}).

%%关卡
-record(res_stage, {
  id::integer(),                              %%id
  desc::string(),                             %%描述
  type::integer(),                            %%类型：1：一次性关卡，2：精英关卡，3:boss关卡
  background_id::string(),                    %%背景图片
  map_id::string(),                           %%地图id
  music_id::string(),                         %%音乐id
  tower_select::list(),                       %%塔
  init_gold::integer(),                       %%初始金币数
  road::integer(),                            %%路id
  monster::list(),                            %%怪物。已不使用该字段
  player_hp::integer(),                       %%玩家血量
  exception_combat::integer(),                %%期望战力
  drop_1::list(),                             %%普通和精英掉落的装备id
  drop_1_prob::integer(),                     %%掉落2件的概率
  drop_2::list(),                          %%掉落3件概率
  drop_2_prob::integer(),                     %%掉落4件概率
  drop_3::list(),                            %%掉落5件的概率
  drop_3_prob::string(),
  boss_drop::string(),                        %%boss掉落描述
  cost_strength::integer(),                   %%消耗体力
  pre_tollgate_id::integer(),                 %%解锁条件
  tollgate_type::integer(),                   %%关卡类型：0:普通关卡，1，特殊关卡， 2，无尽关卡
  cool_time::integer(),                        %%冷却时间
  gain_gold::integer(),                       %%关卡所获金币
  daily_restrict_count::integer(),            %%每日限制次数
  daily_addition_count::integer(),            %%每日额外次数
  normal_cost_energy::[],                     %%通常通关消耗体力 {1,6} proplists
  addition_cost_energy::[],                   %%额外消耗体力 {1，7}
  normal_sweep_energy::[],                    %%通常的扫荡的体力消耗 {1， 7}
  addition_sweep_energy::[],                  %%额外的扫荡的体力消耗  {1， 10}
  all_drops::list()                           %%全部掉落
}).

-record(res_tollgate_energy, {
  tollgate_type::integer(),   %%关卡类型
  normal_energy::[],          %%普通体力消耗
  addition_energy::[],         %%额外体力消耗
  sweep_normal_energy::[],     %%扫荡 普通消耗
  sweep_addition_energy::[]    %%扫荡 额外消耗
}).

%%收成障碍物
-record(res_world_map_block, {
  id::string(),                               %%id
  path::string(),                             %%路径
  armature::string(),                         %%动画
  lock_animation::string(),
  unlock_animation::string(),
  cd_animation::string(),
  unlock_action::string(),
  bind_node::string(),
  interaction_type::integer(),
  unlock_tollgate::integer(),                  %%解锁关卡
  materials::list()                            %%掉落材料
}).


-record(res_exp_config, {
  id::integer(),                               %%等级
  need_exp::integer(),                         %%升级需要
  total_exp::integer()                         %%总经验值
}).

-record(res_login_reward,{
  id::integer(),                               %%连续登陆的天数
  reward_type::integer(),                      %%奖励类型
  reward_number::integer()                     %%奖励数量
}).


-record(res_task, {
  id::string(),                                    %%任务id
  task_type::integer(),                            %%任务类别：1.期限任务，2. 每日任务 3, 活跃度任务
  type_param_1::integer(),                         %%任务类型参数1：主要是不同的任务有不同的id
  type_param_2::string(),                          %%任务类型参数2：目标，根据参数1 的不同设置，有不同的值，
  type_param_3::string(),                          %%任务类型参数2：对象：根据参数1的不同设置有不同的值
  task_name::string(),                             %%任务名称
  task_desc::string(),                             %%任务描述
  reward_type::integer(),                          %%任务奖励类型
  reward_property_id::integer(),                   %%任务奖励道具id
  reward_num::integer(),                           %%奖励数量
  condition_1::integer(),                          %%任务完成条件1
  condition_2::integer(),                          %%任务完成条件2
  daily_limit::integer(),                          %%每日任务次数限制
  daily_refresh::integer(),                        %%是否每日刷新，1：否，2：是
  start_ts::integer(),                             %%期限任务开始时间
  over_ts::integer()                               %%期限任务结束时间
}).

-record(res_achievement, {
  id::string(),
  type_param_1::integer(),                         %%任务类型参数1：主要是不同的任务有不同的id
  group_id::integer(),                             %%成就组id
  level::integer(),                                %%成就等级
  type_param_2::string(),                          %%任务类型参数2：目标，根据参数1 的不同设置，有不同的值，
  type_param_3::string(),                          %%任务类型参数2：对象：根据参数1的不同设置有不同的值
  share_param::integer(),                          %%成就是否需要分享
  name::string(),                                  %%任务名称
  desc::string(),                                  %%任务描述
  reward_type::integer(),                          %%任务奖励类型
  reward_property_id::integer(),                   %%任务奖励道具id
  reward_num::integer(),                           %%奖励数量
  condition_1::integer(),                          %%任务完成条件1
  condition_2::integer()                           %%任务完成条件2
}).

-record(res_goods,{
  id::string(),                                  %%id
  name::string(),                                 %%名称
  pic::string(),                                  %%图片
  goods_no::string(),                             %%物品编号
  goods_type::integer(),                          %%商品类型，1 钻石，2金币
  goods_count::integer(),                         %%商品数量
  money_type::integer(),                          %%消耗的类型：1.钻石，2金币，3 RMB
  money_count::string(),                         %%消耗数量
  original_price::string(),                       %%促销原价
  discount::string(),                             %%折扣限时
  gift_type::integer(),                           %%赠送类型1=钻石，2=金币，3=道具礼包，4=道具，5=体力
  gift_id::string(),                              %%赠送id
  gift_count::integer(),                          %%赠送数量
  restrict_count::integer(),                      %%每天限购的次数（0表示不限购）
  refresh_type::integer(),                        %%商品刷新类型 （0=不限购，1=每天，2=永久）永久不刷新
  is_recommend::integer(),                        %%是否推荐
  start_ts::integer(),                            %%活动开始时间
  over_ts::integer(),                             %%活动结束时间
  promotion_type::integer(),                      %%促销方式
  view_switch::integer()                          %%显示开关
}).

-record(res_shop_package, {
  package_id::string(),             %%包裹id
  package_name::string(),
  package_1_type::integer(),        %%包裹内容的类型： 道具1类型（1=金币，2=钻石，3=道具）
  package_1_id::string(),
  package_1_count::integer(),
  package_2_type::integer(),
  package_2_id::string(),
  package_2_count::integer(),
  package_3_type::integer(),
  package_3_id::string(),
  package_3_count::integer(),
  package_4_type::integer(),
  package_4_id::string(),
  package_4_count::integer(),
  package_5_type::integer(),
  package_5_id::string(),
  package_5_count::integer()
}).

-record(res_activity_reward, {
  id::integer(),                       %%活跃度点
  reward_type::integer(),                          %%任务奖励类型
  reward_property_id::integer(),                   %%任务奖励道具id
  reward_num::integer()                           %%奖励数量
}).

-record(res_property, {
  id::string(),
  name::string(),
  type::integer()
}).

-record(res_template_mail_config,{
  id::string(),
  type::integer(),
  tag::string(),
  title::string(),
  content::string(),
  attach_type::integer(),
  tool_id::string(),
  attach_num::integer(),
  term::integer()
}).

-record(res_treasure_item_config, {
  sub_drop_id::integer(),        %%子id
  item_id::string(),             %%掉落物品id
  item_name::string(),           %%掉落物品名称
  item_type::integer(),          %%掉落物品类型
  item_count::integer(),         %%掉落物品数量
  item_probability::integer(),    %%掉落物品概率
  item_level::integer(),         %%物品等级
  prob_low_value::integer(),    %%概率下限
  prob_up_value::integer()      %%概率上限
}).

-record(res_treasure_config,{
  group_id::integer(),                            %%掉落宝箱id
  item_list::[#res_treasure_item_config{}]    %%掉落宝箱数据
}).

-record(res_endless_config, {
  wave_id::integer(),
  hp_coefficients::float(),     %%生命系数
  drop_chest_id::integer(),
  drop_probability::integer()   %%掉落概率
}).

-record(res_lottery_item_config,{
  lottery_id::integer(),
  lottery_prob::integer(),
  lottery_limit::integer(),
  prob_low_value::integer(),
  prob_up_value::integer()
}).

-record(res_lottery_config,{
  lottery_type::integer(),
  lottery_item_lists::[#res_lottery_item_config{}]
}).

-record(res_lottery_shop_config,{
  lottery_id::integer(),
  sale_id::string(),     %%售卖物品的id
  price::integer(),      %%消耗钻石的数量
  date::string()         %%期限
}).

-record(res_language_config, {
  id::string(),              %%id
  chinese::string(),         %%中文
  english::string()          %%英文
}).

-record(res_equip_upgrade_cost, {
  id::integer(),
  main_cost::integer(),
  main_total::integer(),
  off_cost::integer(),
  off_total::integer(),
  head_cost::integer(),
  head_total::integer()
}).

-record(res_equip_piece, {
  id::string(),
  name::string(),
  star::integer(),
  evolve_id::string(),
  evolve_count::integer()
}).

-record(res_inscription_piece,{
  id::string(),
  name::string(),
  star::integer(),
  evolve_inscription_id::string(),        %%碎片可以合成的铭文id
  belong_tower::string()
}).

-record(res_inscription, {
  id::string(),
  belong_tower::string(),
  evolve_id::string(),          %%可进阶铭文id
  evol_need_piece::string(),    %%进阶至该铭文所需碎片id
  evol_need_count::integer(),   %%所需碎片数量
  compose_piece::string(),      %%合成该铭文所需的碎片id
  compose_count::integer(),     %%合成该铭文所需的碎片数量
  type::integer(),              %%铭文的类型：1、2、3、4 表示在界面上的位置
  star::integer(),              %%铭文品阶
  inscription_class::string()   %%铭文种类
}).

-record(res_tollgate_drop, {
  id::string(),
  treasure_id::integer()
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%平台连续登陆奖励
-record(res_continuous_login_reward, {
  id::integer(),                      %%奖励ID
  reward_player_type::integer(),     %%奖励发放理由：新玩家或者连续登陆，或者所有情况都发
  reward_type::integer(),             %%奖励类型
  reward_deliver_type::integer(),     %%奖励发放方式类型
  reward_prop_id::string(),           %%奖励道具id
  reward_count::integer(),            %%奖励数量
  reward_desc::string(),              %%奖励描述
  reward_start_ts::integer(),         %%发放奖励开启时间
  reward_over_ts::integer()           %%奖励发放结束时间
}).

%%激活码对应的奖励
-record(res_cdkey_package_config,{
  id::string(),
  type::integer(),
  spec_cdkey::string(),
  name::string(),
  c1_type::integer(),
  c1_id::string(),
  c1_count::integer(),
  c2_type::integer(),
  c2_id::string(),
  c2_count::integer(),
  c3_type::integer(),
  c3_id::string(),
  c3_count::integer(),
  c4_type::integer(),
  c4_id::string(),
  c4_count::integer(),
  c5_type::integer(),
  c5_id::string(),
  c5_count::integer()
}).



