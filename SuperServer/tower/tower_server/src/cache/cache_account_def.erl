%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 八月 2014 下午4:08
%%%-------------------------------------------------------------------
-module(cache_account_def).
-author("zqlt").

-include("../csv.hrl").
-include("cache_def.hrl").
-include("../../deps/file_log/include/file_log.hrl").
%% API
-export([
  init_shop/1,
  init_stage/0,
  load_shop_from_config/1,
  create_tower_by_id/2
]).

-define(DEFAULT_ACCOUNT_CONFIG_ID, 1).
-export([init_stage/1]).
-export([init_backpack/2]).
-export([
  init_login_reward/1,
  init_account_from_config/1]).

%%shop
init_shop(InitShop) ->
  Shop = InitShop#shop{goods_list = [], permanent_buy_list = [], last_update_ts = dd_util:timestamp(), pay_info = #pay_info{pay_record = [], first_pay_ts = 0, total_pay_val = 0, total_gem_count = 0}},
  load_shop_from_config(Shop).

load_shop_from_config(Shop) when is_record(Shop, shop) ->
  CommodityList = cache_csv:get_all_shop_config(),
  GoodList =
    lists:foldr(
      fun(Item, TmpGoodList) ->
        case Item#res_goods.refresh_type of
          1 ->  %%每日刷新
            if
              Item#res_goods.restrict_count > 0 ->
                [#goods{id = Item#res_goods.id, remain_count = Item#res_goods.restrict_count, latest_buy_ts = 0} | TmpGoodList];
              true ->
                TmpGoodList
            end;
          _ -> TmpGoodList
        end
      end, [], CommodityList),
  Shop#shop{goods_list = GoodList}.

%%stage
init_stage(Stage) ->
  Stage#stage{base_tollgate_list = [],tollgate_drop = #tollgate_drop{end_ts = 0, seed_ts = 0, seed_val = 0, rand_val = 25583},
  harvest_obstacles_list = [], ac_tollgate = [],
    endless_tollgate = #endless_tollgate{this_week_max_score = 0,max_score = 0, max_wave_count = 0, max_score_gain_ts = 0,
    week_score_record = [], last_update_ts = dd_util:timestamp(),
    this_week_max_score_gain_ts = 0, last_week_max_score = 0, last_week_max_score_gain_ts = 0,
    endless_drop = #endless_drop{endless_drop = [], end_ts = 0, seed_ts = 0, seed_val = 0, rand_val = 25583}}}.

init_stage() ->
  #stage{base_tollgate_list = [], tollgate_drop = #tollgate_drop{end_ts = 0, seed_ts = 0, seed_val = 0, rand_val = 25583},
  harvest_obstacles_list = [], ac_tollgate = [],
  endless_tollgate = #endless_tollgate{this_week_max_score = 0,max_score = 0, max_wave_count = 0, max_score_gain_ts = 0,
  week_score_record = [], last_update_ts = dd_util:timestamp(),
  this_week_max_score_gain_ts = 0, last_week_max_score = 0, last_week_max_score_gain_ts = 0,
  endless_drop = #endless_drop{endless_drop = [], end_ts = 0, seed_ts = 0, seed_val = 0, rand_val = 25583}}}.

%%backpack
init_backpack(InitAccountConfig, Backpack) when is_record(InitAccountConfig, res_init_account) ->
  PropList = dd_util:statistics_list(InitAccountConfig#res_init_account.init_property),
  PropertyTree = cache_api:inc_tree_element(PropList, gb_trees:empty()),
  Backpack#backpack{equipment_list = [], inscription_list = [], material_list = gb_trees:empty(), prop_list = PropertyTree, capacity = InitAccountConfig#res_init_account.init_backpack_capacity
					, s_equipment_list = gb_trees:empty()
				   	, s_material_list = gb_trees:empty()
				   	, s_fragment_list = gb_trees:empty()
				   	, s_consumables_list = gb_trees:empty()
  					, s_card_list = gb_trees:empty()
				   }.

%% init_inscription() ->
%%   InscriptionList = cache_csv:get_all_inscription_config(),
%%   lists:foldl(
%%     fun(Item, TmpList) ->
%%       if
%%         Item#res_inscription.star =:= 5 -> [#inscription{id = Item#res_inscription.id, pos = Item#res_inscription.type} | TmpList];
%%         true -> TmpList
%%       end
%%     end, [], InscriptionList).

%%login reward
init_login_reward(LoginReward) ->
  LoginReward#login_reward{login_times = 1, latest_login_ts = dd_util:timestamp(), login_reward_list = [#login_reward_item{id = 1, login_ts = dd_util:timestamp()}], total_login_days = 1}.

%% init_rewardmatch()->
%% 	#reward_match{fee=0,
%% 								 fee_temp=0,
%% 								 status=0,
%% 								 combo=0,
%% 								 dis_slot=0,
%% 								 level=0,
%% 								 levelmax=0,
%% 								 hp=0,
%% 								 card_list=[],
%% 								 buff_list=[]}.

%%新用户初始化
init_account_from_config(Account) when is_record(Account, account) ->
  InitAccountConfig = cache_csv:get_init_account_cfg(?DEFAULT_ACCOUNT_CONFIG_ID),
  HeroIDList = InitAccountConfig#res_init_account.init_heros,
  HeroList =
    lists:map(
      fun(HeroID) ->
        create_tower_by_id(Account#account.uin, HeroID)
      end, HeroIDList),

  InitSelectHeroList = InitAccountConfig#res_init_account.init_select_hero,
  {_, SelectHero} =
    lists:foldl(
      fun(ID, {Index, TmpList}) ->
        {Index + 1, [{Index, ID} | TmpList]}
      end,{1, []}, InitSelectHeroList),

  %%初始化金币等
  InitGold = InitAccountConfig#res_init_account.init_gold,
  InitGem = InitAccountConfig#res_init_account.init_gem,
  InitStrength = InitAccountConfig#res_init_account.init_strength,

  Account#account{
	players = #players{players_lists = [],select_players = [{0,0},{0,0},{0,0}]},
    heros = #heros{character_lists = HeroList, select_hero = SelectHero},
    gold_coin = InitGold,
    gem = InitGem,
    backpack = cache_account_def:init_backpack(InitAccountConfig, Account#account.backpack),
    strength = Account#account.strength#strength{strength = InitStrength, last_update_ts = dd_util:timestamp(), give_friend_strength = [], close_strength_gift = 1, today_buy_times = 0},
    stage = cache_account_def:init_stage(Account#account.stage),
    shop = cache_account_def:init_shop(Account#account.shop),
    mission = cache_mission:init_mission(Account#account.mission),
    achievement = cache_mission:init_achievement(Account#account.achievement),
    login_reward = cache_account_def:init_login_reward(Account#account.login_reward),
	%reward_match = init_rewardmatch(),
	reward_match = #reward_match{},
	reward_match_ts = #ts_item{count=0,last_update_ts=0}
  }.

create_tower_by_id(Uin, HeroID) ->
  HeroConfig = cache_csv:get_tower_config(dd_util:to_list(HeroID)),
  WeaponNo = HeroConfig#res_tower.base_weapon_id,
  SecondWeaponNo = HeroConfig#res_tower.base_second_weapon_id,
  HatNo =  HeroConfig#res_tower.base_hat_id,

  %%获取唯一ID
  {success, WeaponID} = cache_guid:alloc_guid(Uin),
  {success, SecondWeaponID} = cache_guid:alloc_guid(Uin),
  {success, HatID} = cache_guid:alloc_guid(Uin),

  %%经验为0，等级为1
  Weapon = #equipment{id = WeaponID, no = WeaponNo, exp = 0},
  Second = #equipment{id = SecondWeaponID, no = SecondWeaponNo, exp = 0},
  Hat = #equipment{id = HatID, no = HatNo, exp = 0},

  %%初始技能为空
  #character{id = HeroID, equipment_list = [Weapon, Second, Hat], inscription_list = []}.
