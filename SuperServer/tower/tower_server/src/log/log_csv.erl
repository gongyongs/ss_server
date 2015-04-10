%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 七月 2014 上午10:05
%%%-------------------------------------------------------------------
-module(log_csv).
-author("zqlt").

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
-include("../csv.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-export([
  query_config/2,
  reload_csv/0
]).

-export([
  get_init_account_cfg/1,
  get_tower_config/1,
  get_tollgate_config/1,
  get_drop_config/1,
  get_drop_config_without_exception/1,
  get_equipment_config/1,
  get_equipment_config_without_exception/1,
  get_material_config/1,
  get_world_map_block_config/1,
  get_exp_config/1,
  get_login_reward_config/1,
  get_all_task_config/0,
  get_all_achievement_config/0,
  get_mission_config/1,
  get_commodity_config/1,
  get_all_shop_config/0,
  get_achievement_config/1,
  get_activity_reward_config/1,
  get_property_config/1,
  get_all_mail_template/0,
  get_mail_template_by_id/1,
  get_endless_config_by_wave_id/1,
  get_lottery_config_by_id/1,
  get_lottery_shop_config_by_id/1,
  get_treasure_config_by_id/1,
  get_shop_package_config_by_id/1,
  get_achievement_config_without_exception/1,
  get_mission_config_without_exception/1,
  get_commodity_config_without_exception/1,
  get_lottery_config_without_exception/1,
  get_all_continuous_login_reward_config/0,
  get_equip_upgrade_cost_by_id/1,
  get_cdkey_package_config_by_id/1,
  get_equip_piece_config_by_id/1,
  get_inscription_piece_config_by_id/1,
  get_inscription_by_id/1,
  get_tollgate_drop_by_id/1,
  get_inscription_by_id_without_exception/1,
  get_inscription_piece_config_by_id_without_exception/1
]).

-export([
  get_all_inscription_config/0,
  get_all_inscription_piece_config/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%供rpc调用
query_config(FuncName, ID) when is_atom(FuncName) ->
  try
    cache_csv:FuncName(ID)
  catch
    {custom, Reason} -> {fail, Reason};
    What:Type ->
      ?FILE_LOG_DEBUG("query config error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      {fail, "HintSystemError"}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_tollgate_drop_by_id(ID) when is_list(ID) ->
  case ets:lookup(res_tollgate_drop, ID) of
    [] ->
      ?FILE_LOG_WARNING("get_tollgate_drop_by_id id fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [TollgateDrop] -> TollgateDrop
  end.

get_equip_piece_config_by_id(ID) when is_list(ID) ->
  case ets:lookup(res_equip_piece, ID) of
    [] ->
      ?FILE_LOG_WARNING("get_equip_piece_config_by id fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [EquipPieceConfig] -> EquipPieceConfig
  end.

get_inscription_piece_config_by_id(ID) when is_list(ID) ->
  case ets:lookup(res_inscription_piece, ID) of
    [] ->
      ?FILE_LOG_WARNING("get_inscription_piece_config_by_id fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [InscriptionPieceConfig] -> InscriptionPieceConfig
  end.

get_inscription_by_id(ID) when is_list(ID) ->
  case ets:lookup(res_inscription, ID) of
    [] ->
      ?FILE_LOG_WARNING("get inscription by id fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [InscriptionConfig] -> InscriptionConfig
  end.

get_inscription_piece_config_by_id_without_exception(ID) when is_list(ID) ->
  case ets:lookup(res_inscription_piece, ID) of
    [] ->
      ?FILE_LOG_WARNING("get_inscription_piece_config_by_id fail", []),
      fail;
    [InscriptionPieceConfig] -> {success, InscriptionPieceConfig}
  end.

get_inscription_by_id_without_exception(ID) when is_list(ID) ->
  case ets:lookup(res_inscription, ID) of
    [] ->
      ?FILE_LOG_WARNING("get inscription by id fail", []),
      fail;
    [InscriptionConfig] -> {success, InscriptionConfig}
  end.

get_cdkey_package_config_by_id(ID) when is_list(ID) ->
  case ets:lookup(res_cdkey_package_config, ID) of
    [] ->
      ?FILE_LOG_WARNING("get_cdkey_package_config_by_id fail", []),
      throw({custom, <<"HintInvalidCDKEY">>});
    [CDKeyPackageConfig] -> CDKeyPackageConfig
  end.

get_equip_upgrade_cost_by_id(Level) when is_integer(Level) ->
  case ets:lookup(res_equip_upgrade_cost, Level) of
    [] ->
      ?FILE_LOG_WARNING("get_upgrade equip cost config fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [CostConfig] -> CostConfig
  end.

get_shop_package_config_by_id(PackageNo) when is_list(PackageNo) ->
  case ets:lookup(res_shop_package, PackageNo) of
    [] ->
      ?FILE_LOG_WARNING("get_shop package config fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [PackageConfig] -> PackageConfig
  end.

get_lottery_config_by_id(Id) when is_integer(Id) ->
  case ets:lookup(res_lottery_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get lottery config fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [LotteryConfig] -> LotteryConfig
  end.
get_lottery_shop_config_by_id(Id) when is_integer(Id) ->
  case ets:lookup(res_lottery_shop_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get lottery shop config fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [LotteryShopConfig] -> LotteryShopConfig
  end.
get_treasure_config_by_id(Id) when is_integer(Id) ->
  case ets:lookup(res_treasure_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get treasure config fail, ~p", [Id]),
      throw({custom, <<"HintSystemDataError">>});
    [TreasureConfig] -> TreasureConfig
  end.
get_endless_config_by_wave_id(Id) when is_integer(Id) ->
  case ets:lookup(res_endless_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get endless config fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [EndlessConfig] -> EndlessConfig
  end.
get_mail_template_by_id(Id) when is_list(Id) ->
  case ets:lookup(res_template_mail_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get mail template config fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [TemplateConfig] -> TemplateConfig
  end.
get_property_config(Id) when is_list(Id) ->
  case ets:lookup(res_property_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get property config fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [PropertyConfig] -> PropertyConfig
  end.

get_activity_reward_config(Id) when is_integer(Id) ->
  case ets:lookup(res_activity_reward_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get activity reward config fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [ActivityRewardConfig] -> ActivityRewardConfig
  end.

get_commodity_config(Id) when is_list(Id) ->
  case ets:lookup(res_shop_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get commodity config fail", []),
      throw({custom, <<"HintSystemDataError">>});
    [CommodityConfig] -> CommodityConfig
  end.

get_commodity_config_without_exception(Id) when is_list(Id) ->
  case ets:lookup(res_shop_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get commodity config fail", []),
      {fail, <<"HintSystemDataError">>};
    [CommodityConfig] -> {success, CommodityConfig}
  end.

get_lottery_config_without_exception(Id) ->
  case ets:lookup(res_lottery_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get lottery config fail", []),
      {fail, <<"HintSystemDataError">>};
    [CommodityConfig] -> {success, CommodityConfig}
  end.

get_mission_config(Id) when is_list(Id) ->
  case ets:lookup(res_task_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get task config fail [~p]", [Id]),
      throw({custom, <<"HintSystemDataError">>});
    [TaskConfig] -> TaskConfig
  end.

get_mission_config_without_exception(Id) when is_list(Id) ->
  case ets:lookup(res_task_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get task config fail [~p]", [Id]),
      {fail, "HintSystemDataError"};
    [TaskConfig] -> {success, TaskConfig}
  end.

get_achievement_config_without_exception(Id) when is_list(Id) ->
  case ets:lookup(res_achievement_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get achievement config fail [~p]", [Id]),
      {fail, "HintSystemDataError"};
    [AchievementConfig] -> {success, AchievementConfig}
  end.

get_achievement_config(Id) when is_list(Id) ->
  case ets:lookup(res_achievement_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get achievement config fail [~p]", [Id]),
      throw({custom, <<"HintSystemDataError">>});
    [AchievementConfig] -> AchievementConfig
  end.

get_login_reward_config(Id) when is_integer(Id) ->
  case ets:lookup(res_login_reward_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get login reward fail [~p]", [Id]),
      throw({custom, <<"HintSystemDataError">>});
    [RewardConfig] -> RewardConfig
  end.

get_exp_config(Id) when is_integer(Id) ->
  case ets:lookup(res_exp_config,Id) of
    [] ->
      ?FILE_LOG_ERROR("get exp config fail [~p]", [Id]),
      throw({custom, <<"HintSystemDataError">>});
    [ExpConfig] -> ExpConfig
  end.

get_world_map_block_config(Id) when is_list(Id) ->
  case ets:lookup(res_world_map_block, Id) of
    [] ->
      ?FILE_LOG_WARNING("get_world_map config fail [~p]", [Id]),
      throw({custom, <<"HintSystemDataError">>});
    [BlockConfig] -> BlockConfig
  end.

get_material_config(Id) when is_list(Id) ->
  case ets:lookup(res_material_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get_material config fail [~p]", [Id]),
      throw({custom, <<"HintSystemDataError">>});
    [MaterialConfig] -> MaterialConfig
  end.
get_equipment_config(Id) when is_list(Id) ->
  case ets:lookup(res_equip_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get equipment config fail [~p]", [Id]),
      fail;
%      throw({custom, <<"HintSystemDataError">>});
    [EquipmentConfig] -> EquipmentConfig
  end.

get_equipment_config_without_exception(Id) when is_list(Id) ->
  case ets:lookup(res_equip_config, Id) of
    [] ->
      ?FILE_LOG_WARNING("get equopment config fail [~p]", [Id]),
      {fail, "HintSystemDataError"};
    [EquipmentConfig] -> {success, EquipmentConfig}
  end.

get_init_account_cfg(Id) when is_integer(Id) ->
  case ets:lookup(res_init_account, Id) of
    [] ->
      ?FILE_LOG_WARNING("get init account [~p] fail", [Id]),
      throw({custom, <<"HintSystemDataError">>});
    [InitAccountVal] -> InitAccountVal
  end.

get_tower_config(HeroId) when is_list(HeroId) ->
  case ets:lookup(res_tower_config, HeroId) of
    [] ->
      ?FILE_LOG_WARNING("get tower config [~p] fail", [HeroId]),
      throw({custom, <<"HintSystemDataError">>});
    [TowerConfig] -> TowerConfig
  end.

get_tollgate_config(TollgateID) when is_integer(TollgateID) ->
  case ets:lookup(res_stage, TollgateID) of
    [] ->
      ?FILE_LOG_ERROR("get tollgate config [~p] fail", [TollgateID]),
      throw({custom, <<"HintSystemDataError">>});
    [TollgateConfig] -> TollgateConfig
  end.

get_drop_config(DropID) when is_list(DropID) ->
  case ets:lookup(res_drop_index_table, DropID) of
    [] ->
      ?FILE_LOG_ERROR("get drop config [~p] fail", [DropID]),
      throw({custom, <<"HintSystemDataError">>});
    [DropConfig] -> DropConfig
  end.

get_drop_config_without_exception(DropID) when is_list(DropID) ->
  case ets:lookup(res_drop_index_table, DropID) of
    [] ->
      ?FILE_LOG_ERROR("get drop config [~p] fail", [DropID]),
      {fail, "HintSystemDataError"};
    [DropConfig] -> {success, DropConfig}
  end.

get_all_task_config() ->
  ets:tab2list(res_task_config).

get_all_achievement_config() ->
  ets:tab2list(res_achievement_config).

get_all_shop_config() ->
  ets:tab2list(res_shop_config).

get_all_mail_template() ->
  ets:tab2list(res_template_mail_config).

get_all_continuous_login_reward_config() ->
  ets:tab2list(res_continuous_login_reward).

get_all_inscription_config() ->
  ets:tab2list(res_inscription).

get_all_inscription_piece_config() ->
  ets:tab2list(res_inscription_piece).

reload_csv() ->
  gen_server:call(?MODULE, reload_csv).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  load_csv(),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(reload_csv, _From, State) ->
  Ret = reload_csv_to_cache(),
  {reply, Ret, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_csv() ->
  CsvPath = get_csv_path(),
  csv:load_csv_to_cache(CsvPath).

reload_csv_to_cache() ->
  try
    CsvPath = get_csv_path(),
    csv:reload_csv_to_cache(CsvPath),
    success
  catch
    What:Type ->
      ?FILE_LOG_ERROR("reload csv error, what = ~p, type = ~p, stack = ~p", [What, Type, erlang:get_stacktrace()]),
      fail
  end.

get_csv_path() ->
  CsvPath = os:getenv("CVS_PATH"),
  {success, PlatformName} = dd_ms:read_config(platform_name),
  {success, PlatformType} = dd_ms:read_config(platform_type),
  CsvPath ++ dd_util:to_list(PlatformName) ++ "_" ++ dd_util:to_list(PlatformType) ++ "/".