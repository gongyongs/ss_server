%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 七月 2014 上午10:32
%%%-------------------------------------------------------------------
-author("zqlt").
-define(RAND_NUMBER, 25583).

-define(TICKET_RECOVER_TIME, (8*60*60)).
-define(TICKET_MAX, 5).

-define(RATE_BOX, 100).
-define(RATE_BUFF, 100).
-define(RATE_BATTLE, 100).

-define(RATE_BOSS, 10).
-define(RATE_ELITE, 10).
-define(RATE_MONSTER, 100).

-define(DEFAULT_SLOT, 3).
-define(DEFAULT_HID_SLOT, 9).
-define(BOX_FEE, 100).

-define(RM_STATUS_CLOSE, 0).
-define(RM_STATUS_OPEN, 1).
-define(RM_STATUS_BATTLE, 2).

-define(ID_BUFF_MULTI, 1).
-define(BUFF_SLOT, 3).%buff列表固定长度

-record(record_item, {
  key, value
}).

-record(friend_item, {
  uin::integer(),
  id::string(),
  dis_name::string()
}).

-record(platform_info,{
  plat_type::string(),
  player_id::string(),
  player_dis_name::string(),
  player_friends::[#friend_item{}]
}).

-record(equipment, {
  id::string(),         %%装备全服唯一id
  no::string(),         %%装备编号
  exp::integer()       %%当前经验
}).

%%铭文
-record(inscription, {
  pos::integer(),
  id::string()
}).

%%角色
-record(character, {
  id::string(),
  equipment_list::[#equipment{}],
  inscription_list::[#inscription{}]                  %%已装备的铭文
}).

-record(heros, {
  character_lists::[#character{}],
  select_hero::[]         %%选中的角色列表{1, "TO1"}  {2, "TO2"}
}).

-record(players, {
  players_lists::[#character{}],
  select_players::[]         %%选中的角色列表{1, "TO1"}  {2, "TO2"}
}).


%% -record(material,{
%%   id::string(),       %%材料编号
%%   count::integer()    %%数量
%% }).
%%
%% %%碎片相关
%% -record(piece, {
%%   id::string(),       %%材料编号
%%   count::integer()    %%数量
%% }).
%%
%% %%道具
%% -record(property, {
%%   id::string(),
%%   count::integer()
%% }).
%%背包
-record(backpack, {
  equipment_list::[#equipment{}],                   %%装备（2014.12.20已取消使用）
  material_list,                          %%材料 （2014.12.20 已改為裝備碎片） gb_trees()
  equip_piece,                            %%材料   gb_trees()
  prop_list,                                          %%道具  gb_trees()
  inscription_piece_list,                             %%碎片銘文 gb_trees()
  inscription_list::[#inscription{}],               %%铭文
  capacity::integer(),                                %%容量
  %superstar信息
  s_equipment_list,%玩家装备
  s_material_list,%合成材料
  s_fragment_list,%球员碎片
  s_consumables_list,%消耗品
  s_card_list%图鉴
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(mission_item, {
  mission_id::string(),             %%任务id
  mission_type::integer(),          %%任务类型，每日任务，期限任务，活跃度任务
  mission_progress_1::integer(),      %%任务进度，对于完成任务条件的condition1
  mission_progress_2::integer(),     %%对于完成条件中的condition2
  mission_record::[#record_item{}],               %%某些任务需要统计每一次数据更新的情况
  mission_finish_times::integer(),                   %%单天完成次数，仅限于活跃度任务
  mission_finish_ts::integer(),     %%任务完成时间
  mission_get_reward_ts::integer()  %%任务领取奖励的时间
}).

%%活跃度奖励选项
-record(activity_reward_item, {
  id::integer(),
  finish_ts::integer(),
  get_reward_ts::integer()
}).
-record(player_activity,{
  activity_value::integer(),                                  %%玩家活跃度
  activity_reward_list::[#activity_reward_item{}]
}).
%%任务
-record(mission, {
  mission_list::[#mission_item{}],
  player_activity::#player_activity{}       %%玩家活跃度
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(achievement_item,{
  id::string(),
  statistics::integer(),                  %%进度,根据条件1统计
  statistics_1::integer(),                %%根据条件2的统计
  progress::integer(),                    %%进度百分比 (2014.12.26 修改为 显示的进度值)
  record::[#record_item{}],             %%相关记录   {id, count}
  finish_ts::integer(),                   %%完成成就时间
  get_reward_ts::integer()                %%领取奖励时间
}).

-record(achievement_group,{
  group_id::integer(),
  item_level_1::#achievement_item{},
  item_level_2::#achievement_item{},
  item_level_3::#achievement_item{}
}).

%%成就
-record(achievement, {
  group_list::[#achievement_group{}],     %%成就列表
  last_update_ts::integer()                  %%上次更新成就的时间
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%商城售卖项目
-record(goods, {
  id::string(),
  remain_count::integer(),           %%购买次数
  latest_buy_ts::integer()    %%最新一次的购买时间
}).

%%商店
-record(pay_item,{
  pay_ts::integer(),
  pay_item_id::string(),
  pay_gem_count::integer(),
  pay_price::float()    %%dollar 美元，实际上是float
}).

-record(pay_info,{
  first_pay_ts::integer(),
  pay_record::[#pay_item{}],
  total_pay_val::float(),
  total_gem_count::integer()
}).

-record(shop, {
  goods_list::[#goods{}],            %%每天刷新有限制次数的
  permanent_buy_list::[#goods{}],    %%永久购买的不刷新
  pay_info::#pay_info{},
  last_update_ts::integer()
}).

-record(login_reward_item, {
  id::integer(),            %%登陆的次数，第几次
  login_ts::integer()      %%登陆的时间
}).
%%登陆奖励
-record(login_reward, {
  latest_login_ts::integer(),
  login_times::integer(),
  total_login_days::integer(),
  login_reward_list::[#login_reward_item{}]
}).

-record(strength_item,{
  give_strength_value::integer(),   %%赠送体力值
  give_strength_source::integer(),  %%赠送体力者
  give_strength_dest::integer(),    %%被赠送者
  give_strength_ts::integer()       %%赠送时间
}).

%%体力值
-record(strength, {
  strength::integer(),
  last_update_ts::integer(),   %%上一次恢复体力的时间 (按时间回复的体力)
  %%receive_friend_strength::[#strength_item{}],  %%接受好友体力赠送记录
  give_friend_strength::[#strength_item{}],      %%赠送好友体力记录
  close_strength_gift::integer(),              %%关闭好友赠送 0关闭 1开启
  today_buy_times::integer()                  %%今天已购买的次数
}).
-record(tollgate,{
  id::integer(),
  max_star::integer(),         %%获取星级
  max_score::integer(),
  daily_remain_times::integer(),  %%当天剩余次数
  addition_remain_times::integer(), %%额外剩余天数
  last_pass_ts::integer(),      %%最近一次通关的时间
  cool_time::integer(),        %%冷却时间（配置数据）
  max_star_seq::list()        %%获得的星级序列： [-1, 0, -1] 0表示获得第二颗星, -1表示未获得该颗星
}).

%%活动关卡的所有数据
-record(activity_tollgate_item, {
  tollgate_id::integer(),
  max_score::integer(),
  max_star::integer(),
  last_finish_ts::integer(),        %%上一次通关时间，统计冷却时间
  daily_remain_times::integer(),     %%每天剩余次数
  cool_time::integer(),        %%冷却时间（配置数据）
  max_star_seq::[]             %% 获得的星级序列： [-1, 0, -1] 0表示获得第二颗星，-1表示未获得该颗星
}).

-record(harvest_obstacles_item, {
  id::string(),                     %%id
  remain_number::integer(),         %%剩余次数
  last_update_ts::integer()         %%最近一次的更新时间
}).

-record(treasure_item,{
  treasure_id::integer(),   %%宝箱id
  sub_id::integer(),     %%子id
  drop_id::string(),     %%物品no
  drop_type::integer(),  %%物品类型
  count::integer(),       %%物品数量
  drop_level::integer()   %%物品等级
}).

-record(endless_drop,{
  seed_val::integer(),                  %%随机种子值
  rand_val::integer(),                  %%随机值
  seed_ts::integer(),                   %%随机种子时间
  end_ts::integer(),                    %%随机种子无效期
  endless_drop::[#treasure_item{}] %%今天掉落的宝箱
}).

-record(tollgate_drop,{
  seed_val::integer(),                  %%随机种子值
  rand_val::integer(),                  %%随机值
  seed_ts::integer(),                   %%随机种子时间
  end_ts::integer()                    %%随机种子无效期
}).

-record(endless_item, {
  time::integer(),
  score::integer(),
  tollgate_id::integer()
}).
-record(endless_tollgate, {
  max_score::integer(),                   %%历史最高分
  max_score_gain_ts::integer(),           %%历史最高分获得时间
  max_wave_count::integer(),              %%历史最高无尽波数
  last_week_max_score::integer(),         %%上周最高分
  last_week_max_score_gain_ts::integer(), %%上周最高分的获取时间
  this_week_max_score::integer(),         %%本周最高分
  this_week_max_score_gain_ts::integer(), %%本周最高分的获取时间
  last_update_ts::integer(),              %%上次更新的时间
  week_score_record::[#endless_item{}], %%本周通关记录(目前每日更新)
  endless_drop::#endless_drop{}         %%无尽掉落(每天的掉落，每日更新)
}).

%%关卡
-record(stage, {
  base_tollgate_list::[#tollgate{}],                        %%所有基础配置的关卡
  tollgate_drop::#tollgate_drop{},                         %%闯关模式的掉落
  endless_tollgate::#endless_tollgate{},
  ac_tollgate::[#activity_tollgate_item{}],               %%活动关卡
  harvest_obstacles_list::[#harvest_obstacles_item{}]     %%收成障碍物
}).

%%附加信息
-record(addition,{
  newer_guide_steps::integer(),     %%新手引导第几步
  guide_steps::[#record_item{}],
  rank_reward_got::integer(),       %%奖励显示，0 未显示，1 已显示
  %%礼包领取
  cdkey_pack_list::any()        %%礼包领取列表,用于处理某种礼包只能领取一个  (gb_trees)
}).

-record(lottery_conf,{
  id::integer(),
  tool_id::string(),
  price::integer(),
  is_discount::integer(),
  start_ts::integer(),
  over_ts::integer()
}).

-record(lottery, {
  today_lottery_lists::[#treasure_item{}],
  single_lottery_times::integer(),
  ten_lottery_times::integer(),
  last_lottery_ts::integer()
}).

-record(guild, {

}).
-record(competitive,{

}).


%根据时间更新的结构（目前只有赏金赛）
-record(ts_item, {
  count::integer(),		%赏金赛入场券
  last_update_ts::integer()   	%上次入场时间（时间戳：秒）
}).


-record(rm_card,{
	index::integer(),	%序号
	type::integer(),		%种类(0.宝箱，1.状态，2.战斗)
	id::integer()		%对应种类的配置ID				 
}).

-record(rm_buff,{
	index::integer(),	%序号
	id::integer(),		%对应种类的配置ID	
	type::integer()		%种类		 
}).

-record(rm_combo,{
	level::integer(),		%难度等级
	max::integer()		%历史最大连胜数		 
}).

-record(reward_match,{
	fee=0,		%赏金
	fee_temp=0,	%赏金(临时，单场)
	fee_battle=0,%上一场战斗赏金(临时，单场)
	status=0,	%状态（0.未进场，1.进场，2.战斗）	
    battle_id=0,%当前战斗id
	select_index = 0,%当前选中卡索引	
	max_index = 0,%当前最大卡牌索引	
	max_index_buff = 0,%当前最大buff索引	  	
	combo=0,   	%连胜场次
	combomax = [], %连胜历史记录
	dis_slot=0,	%减少可见数
	level=0,	%当前选择等级
	levelmax=0,	%最大开启等级
	levelcount = [], %历史场次
	hp=0,		%当前生命值
	hpmax=1000,	%最大生命值
	card_list=[],	%可见卡
	card_list_hid=[], %不可见卡
	buff_list=[],	%当前buff
	
	reward_team = [{0,0},{0,0},{0,0}]	%%当前赏金队伍信息				 
}).


%%玩家信息
-record(account, {
  uin::integer(),
  gold_coin::integer(),
  gem::integer(),
  platform_info::#platform_info{},
  heros::#heros{},
  players,						%%玩家球员信息
  backpack::#backpack{},
  mission::#mission{},
  achievement::#achievement{},
  shop::#shop{},
  stage::#stage{},
  login_reward::#login_reward{},
  strength::#strength{},
  lottery::#lottery{},
  guild::#guild{},        %%公会
  competitive::#competitive{}, %%竞技
  addition::#addition{},
  create_ts::integer(),
  last_login_ts::integer(),
  reward_match::#reward_match{},	%%赏金赛信息
  reward_match_ts::#ts_item{}		%%赏金赛入场券信息
}).

-record(kill_monster, {
  normal_monster_list::[],     %% {id, number}
  normal_monster_number::integer(),
  elite_monster_list::[],
  elite_monster_number::integer(),
  boss_monster_list::[],
  boss_monster_number::integer()
}).

-record(statistics_item, {
  tower_id::string(),
  build_num::integer(),
  sell_num::integer(),
  kill_obstacles::integer(),
  use_skill_num::integer(),
  total_kill_monster::integer(),
  kill_monster::#kill_monster{}
}).

%%游戏结束数据
-record(game_end_statistics, {
  monster_list::[#statistics_item{}],
  total_kill_all_monster::integer(),
  total_kill_normal_monster::integer(),
  total_kill_elite_monster::integer(),
  total_kill_boss_monster::integer(),
  total_build_tower::integer(),
  total_kill_obstacles::integer(),
  total_use_skill::integer()
}).

-record(game_end, {
  success::integer(),    %%0 成功，1失败
  tollgate_id::integer(),
  endless_tollgate_num::integer(),      %%无尽模式通过多少关
  gain_star::integer(),
  gain_star_seq::list(),   %%[0, 1, 0]
  gain_score::integer(),
  gain_gold::integer(),
  use_props::list(),
  jelly_consume_num::integer(),       %%消耗果冻数量
  gain_drops::list(),
  data_statistics::#game_end_statistics{}
}).

-record(strengthen_data, {
  type::integer(), %% 0表示升级， 1表示进阶
  target_level::integer(), %%目标等级
  target_id::string(),     %%目标id
  target_no::string(),     %%目标编号
  equipment_list::[]       %%玩家所有装备的列表
}).

-record(notice_rd, {
  notice_id::integer(),
  notice_title::string(),
  notice_date::string(),
  notice_detail::string(),
  notice_pic_url::string(),
  notice_sign::string()     %%落款
}).

