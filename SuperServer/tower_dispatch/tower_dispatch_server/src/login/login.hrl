%% return string()
-define(LOGIN_VERSION, element(2, application:get_key(login, vsn))).
-define(LOGIN_DESCRIPTION, element(2, application:get_key(login, description))).

-record(login_cfg, {key, value}).



-record(friend_item,{
  id::string(),
  dis_name::string()
}).

-record(friends, {
  friend_list::[#friend_item{}],
  last_update_ts::integer()
}).

-record(platform, {
  plat_type::string(),              %%平台类型
  plat_friend::[#friend_item{}],  %%平台好友
  player_id::string(),              %%玩家id
  player_dis_name::string()         %%玩家显示名
}).

-record(addition, {

}).

-record(user_info,
{
  device::string(),
  uin::integer(),
  uname::string(),
  dis_name::string(),
  platform_info::#platform{},
  addition::#addition{},
  create_ts::integer(),
  add_ts::integer()
}).