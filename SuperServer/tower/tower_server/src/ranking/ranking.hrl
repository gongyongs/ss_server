%% return string()
-define(RANKING_VERSION, element(2, application:get_key(ranking, vsn))).
-define(RANKING_DESCRIPTION, element(2, application:get_key(ranking, description))).

-record(rank_info,{
  uin::integer(),           %%玩家唯一标示
  uname::string(),          %%玩家的平台id
  dis_name::string(),       %%玩家名字
  score::integer(),         %%分数
  gain_score_ts::integer(), %%获取分数的时间
  rank::integer()           %%排名
}).

-record(endless_info, {
  max_score::integer(),
  gain_max_ts::integer(),
  this_score::integer(),
  gain_this_ts::integer(),
  last_score::integer(),
  gain_last_ts::integer()
}).

-record(ranking_tollgate_info,{
  uin::integer(),
  uname::string(),       %%玩家平台id
  dis_name::string(),    %%玩家名字
  tg_tree,       %%基础关卡或者活动关卡
  endless::#endless_info{}           %%无尽关卡
}).

-record(rank_tg,{
  tg_id::integer(),     %%关卡id
  tg_type::integer(),   %%1 普通关卡  2 无尽关卡  3 活动关卡
  tg_max_score::integer(),
  tg_gain_ts::integer()   %%获取分数的时间
}).

