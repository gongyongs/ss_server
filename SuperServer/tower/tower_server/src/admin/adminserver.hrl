%% return string()
-define(MYTEST_VERSION, element(2, application:get_key(adminserver, vsn))).
-define(MYTEST_DESCRIPTION, element(2, application:get_key(adminserver, description))).

-define(POST, 'POST').
-define(GET, 'GET').
%-define(SQL_NAME, dd_sql).

-record(admin_player_data,
{
  name::string(),
  uin::integer(),
  plat::string(),              %%注册来源
  register_date::string(),     %%注册时间
  atk::integer(),                %%战斗力
  gem::integer(),               %%宝石
  bind_gem::integer(),          %%绑定宝石
  gold::integer(),
  strength::integer(),
  tollgate_count::integer(),     %%当前关卡
  total_star::integer(),         %%总星数
  endless_max_score::integer(),  %%无尽模式最高分数
  endless_max_count::integer(),  %%无尽模式最高波次
  pay_count::string(),           %%充值金额
  pay_gem_count::integer()       %%充值宝石
}).

-record(admin_equip_item,{
  equip_id::string(),
  equip_no::string(),
  equip_name::string(),
  equip_level::integer(),
  equip_star::integer(),
  equip_atk::integer(),
  equip_type::integer()
}).

-record(admin_material_item,{
  material_id::string(),
  material_name::string(),
  material_count::integer()
}).

-record(admin_property_item,{
  prop_id::string(),
  prop_name::string(),
  prop_count::integer()
}).

-record(admin_inscription_item, {
  inscription_id::string(),
  inscription_type::integer(),
  inscription_star::integer()
}).

-record(admin_tower_item,{
  tower_no::string(),
  tower_name::string(),
  tower_atk::string(),
  tower_equip_list::[#admin_equip_item{}],
  tower_inscription_list::[#admin_inscription_item{}]
}).

-record(admin_tower_info,{
  tower_list::[#admin_tower_item{}]
}).

-record(admin_backpack_item,{
  item_id::string(),       %%id
  item_type::integer(),    %%1 装备， 2表示材料。3 表示道具
  item_name::string(),
  item_count::integer()
}).

-record(admin_inscription_piece_item, {
  inscription_piece_id::string(),
  inscription_piece_name::string(),
  inscription_piece_star::integer(),
  inscription_piece_count::integer()
}).

-record(admin_superstar_item, {
	uuid::string(),
  	id::integer(),
  	count::integer()
}).

-record(admin_backpack,{
  equip_list::[#admin_equip_item{}],
  material_list::[#admin_material_item{}],
  property_list::[#admin_property_item{}],
  inscription_list::[#admin_inscription_item{}],
  inscription_piece_list::[#admin_inscription_piece_item{}],
  capacity::integer(),
  %superstar属性
  s_equipment_list::[#admin_superstar_item{}],
  s_material_list::[#admin_superstar_item{}],
  s_fragment_list::[#admin_superstar_item{}],
  s_consumables_list::[#admin_superstar_item{}],
  s_card_list::[#admin_superstar_item{}]
}).
