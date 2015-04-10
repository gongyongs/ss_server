-module(cache).
-author('erlangonrails@gmail.com').
-export([start/0, stop/0]).
-include("../../deps/file_log/include/file_log.hrl").
-export([
  login/2,
  get_user_info/2,
  change_team/2,
  game_end/2,
  replace_equipment/4,
  sell_material/2,
  click_world_map_block/3,
  strengthen/2,
  add_material/2,
  get_login_reward/2,
  buy_goods/3,
  get_activity_reward/2,
  get_achievement_reward/3,
  get_mission_reward/2,
  reset_account/1,
  load_mail_template_config/0,
  get_mail_template_config/1,
  present_friend_strength/2,
  sync_user_info/2,
  get_endless_germ/1,
  lottery/2,
  get_mail_attach/2,
  iap_buy/4,
  share_score/2,
  settlement_friend_rank_reward/2,
  settlement_server_rank_reward/4,
  fast_purchase/3,
  sync_guide_step/3,
  get_user_basic_info/2,
  query_account/2,
  get_mass_attach_mail/2,
  get_server_endless_rank/1,
  get_friend_endless_rank/1,
  exchange_cdkey/2,
  compose_inscription/2,
  replace_inscription/5,
  enter_tollgate/2,
  advance_inscription/3,
  update_tower_team/2,
  tollgate_sweep/3,
  op_bag/3,
  op_bag_add/4,
  op_bag_del/4,
  op_rewardmatch/3,
  op_ladder/3
]).

-export([
  generate_order/4,
  complete_orders/3
]).

-export([
  stat_power_rank/2,
  stat_pay_rank/2
]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(cache).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(cache).

login(Uin, LoginInfo) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, login, {Uin, LoginInfo}).

get_user_info(Uin, TypeList) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_user_info, {Uin, TypeList}).

game_end(Uin, GameEndStatList) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, game_end, {Uin, GameEndStatList}).

replace_equipment(Uin, TowerID, EuipmentID, ReplaceID) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, replace_equipment, {Uin, TowerID, EuipmentID, ReplaceID}).

sell_material(Uin, Param) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, sell_material, {Uin, Param}).

click_world_map_block(Uin, WBlockID, MaterialList) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, click_world_map_block, {Uin, WBlockID, MaterialList}).

strengthen(Uin, Param) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, strengthen, {Uin, Param}).

get_login_reward(Uin, LoginTimes) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_login_reward, {Uin, LoginTimes}).

buy_goods(Uin, CommodityId, Ip) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, buy_goods, {Uin, CommodityId, Ip}).

get_activity_reward(Uin, ActivityID) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_activity_reward, {Uin, ActivityID}).

get_mission_reward(Uin, MissionId) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_mission_reward, {Uin, MissionId}).

get_achievement_reward(Uin, GroupId, AchievementID) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_achievement_reward, {Uin, GroupId, AchievementID}).

present_friend_strength(Uin, FriendUin) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, present_friend_strength, {Uin, FriendUin}).

sync_user_info(Uin, DataList) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, sync_user_info, {Uin, DataList}).

lottery(Uin, LotteryType) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, lottery, {Uin, LotteryType}).

get_endless_germ(Uin) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_endless_germ, Uin).

iap_buy(Uin, Orders, ProductID, Receipt) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, iap_buy, {Uin, Orders, ProductID, Receipt}).

share_score(Uin, Plat) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, share_score, {Uin, Plat}).

settlement_server_rank_reward(Uin, Percent, Rank, Point) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
    cache_work:execute(ProcName, Uin, settlement_server_rank_reward, {Uin, Percent, Rank, Point}).

settlement_friend_rank_reward(Uin, RankItem) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, settlement_friend_rank_reward, {Uin, RankItem}).

fast_purchase(Uin, PurchaseType, Value) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, fast_purchase, {Uin, PurchaseType, Value}).

sync_guide_step(Uin, StepID, StepVal) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, sync_guide_step, {Uin, StepID, StepVal}).

get_user_basic_info(Uin, UinList) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_user_basic_info, {Uin, UinList}).

get_mass_attach_mail(Uin, Type) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_mass_attach_mail, {Uin, Type}).

get_server_endless_rank(Uin) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_server_endless_rank, Uin).

get_friend_endless_rank(Uin) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_friend_endless_rank, Uin).

exchange_cdkey(Uin, CDKeyID) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, exchange_cdkey, {Uin, CDKeyID}).

compose_inscription(Uin, InscriptionID) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, compose_inscription, {Uin, InscriptionID}).

replace_inscription(Uin, TowerID, InscriptionID, ReplaceID, Pos) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, replace_inscription, {Uin, TowerID, InscriptionID, ReplaceID, Pos}).

update_tower_team(Uin, TowerTeam) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, update_tower_team, {Uin, TowerTeam}).

enter_tollgate(Uin, TollgateID) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, enter_tollgate, {Uin, TollgateID}).

advance_inscription(Uin, TowerID, InscriptionID) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, advance_inscription, {Uin, TowerID, InscriptionID}).

tollgate_sweep(Uin, TollgateID, SweepTimes) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, tollgate_sweep, {Uin, TollgateID, SweepTimes}).

%%%%%%%%%%平台
complete_orders(Uin, PlatType, OrderInfo) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, platform_oper, {complete_orders, {Uin, PlatType, OrderInfo}}).

generate_order(Uin, BillNo, GoodsID, Plat) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, platform_oper, {generate_order, Plat, {Uin, BillNo, GoodsID}}).

%%测试接口
add_material(Uin, Param) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, add_material, {Uin, Param}).

reset_account(Uin) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, reset_account, Uin).

get_mail_attach(Uin, MailID) ->
  ProcName = cache_sup:hash_uin_to_proc(Uin),
  cache_work:execute(ProcName, Uin, get_mail_attach, {Uin, MailID}).

op_bag(Uin,OP,Params) ->
	ProcName = cache_sup:hash_uin_to_proc(Uin),
  	cache_work:execute(ProcName, Uin, op_bag, {Uin,OP,Params}).

op_bag_add(Uin,Params,Addcount,Ip) ->
	ProcName = cache_sup:hash_uin_to_proc(Uin),
  	cache_work:execute(ProcName, Uin, op_bag_add, {Uin,Params,Addcount,Ip}).
op_bag_del(Uin,UUID,Delcount,Ip) ->
	ProcName = cache_sup:hash_uin_to_proc(Uin),
  	cache_work:execute(ProcName, Uin, op_bag_del, {Uin,UUID,Delcount,Ip}).

op_rewardmatch(Uin,OP,Params) ->
	ProcName = cache_sup:hash_uin_to_proc(Uin),
  	cache_work:execute(ProcName, Uin, op_rewardmatch, {Uin,OP,Params}).

op_ladder(Uin,OP,Params) ->
	ProcName = cache_sup:hash_uin_to_proc(Uin),
  	cache_work:execute(ProcName, Uin, op_ladder, {Uin,OP,Params}).

%%更新球员队伍
change_team(Uin,SuperTeam)->
	ProcName = cache_sup:hash_uin_to_proc(Uin),
  	cache_work:execute(ProcName,Uin,change_team, {Uin,SuperTeam}).


load_mail_template_config() ->
  try
    TemplateList = cache_csv:get_all_mail_template(),
    {success, TemplateList}
  catch
    {custom, Reason} -> {fail, dd_util:to_list(Reason)};
    What:Type ->
      ?FILE_LOG_ERROR("load_mail_template_config error, [what = ~p, type = ~p, stack = ~p]", [What, Type, erlang:get_stacktrace()]),
      {fail, "logic error"}
  end.

get_mail_template_config(TemplateID) ->
  try
    TemplateConfig = cache_csv:get_mail_template_by_id(TemplateID),
    {success, TemplateConfig}
  catch
    {custom, Reason} -> {fail, dd_util:to_list(Reason)};
    What:Type ->
      ?FILE_LOG_ERROR("get_mail_template_config error, [what = ~p, type = ~p, stack = ~p]", [What, Type, erlang:get_stacktrace()]),
      {fail, "logic error"}
  end.




stat_pay_rank(Index, Len) ->
  cache_data_mgr:stat_pay_rank(Index, Len).

stat_power_rank(Index, Len) ->
  cache_data_mgr:stat_power_rank(Index, Len).

query_account(Uin, FieldList) when is_integer(Uin) andalso is_list(FieldList) ->
  cache_data_mgr:query_account_without_create(Uin, FieldList).

