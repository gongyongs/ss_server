%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 十月 2014 上午11:31
%%%-------------------------------------------------------------------
-module(cache_configure_data).
-author("zqlt").

-behaviour(gen_server).

-include("../../deps/file_log/include/file_log.hrl").
-include("../csv.hrl").
-include("../cache/cache_def.hrl").

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
  add_shop_goods/1,
  get_commodity_config/1,
  get_commodity_from_ets/0,    %%获取数据库中的打折商品信息
  query_all_shop_goods/0,
  del_commodity_config/1,
  get_lottery_config_data/1,
  reload_lottery_config_data/0
]).

%%%===================================================================
%%% API
%%%===================================================================
get_commodity_from_ets()->
  CommodityList = ets:tab2list(shop_config_data),
  {_UnStartList, _OutOfDateList, OnGoingList} = filter_commodity_out_of_date(CommodityList),
  OnGoingList.


get_commodity_config(ID) when is_list(ID) ->
  case gen_server:call(?MODULE, {get_commodity_config, ID}) of
    {success, Commodity} -> Commodity;
    {fail, _ } -> throw({custom, "HintSystemDataError"})
  end.

add_shop_goods(GoodItem) when is_record(GoodItem, res_goods)->
  gen_server:call(?MODULE, {add_shop_goods, GoodItem}).

query_all_shop_goods() ->
  {success, {cache_csv:get_all_shop_config(), ets:tab2list(shop_config_data)}}.

del_commodity_config(ID) when is_list(ID) ->
  gen_server:call(?MODULE, {del_commodity_config, ID}).

get_lottery_config_data(ID) when is_integer(ID)->
  gen_server:call(?MODULE, {get_lottery_config_data, ID}).

reload_lottery_config_data() ->
  gen_server:call(?MODULE, reload_lottery_config).

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
  ets:new(shop_config_data, [set, protected, named_table, {keypos, #res_goods.id}]),
  ets:new(lottery_config_data, [set, protected, named_table, {keypos, #lottery_conf.id}]),
  erlang:start_timer(1*10000, self(), 'start_load_config'),
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
handle_call({add_shop_goods, ShopItem}, _From, State) ->
  Ret =
    case ets:lookup(shop_config_data, ShopItem#res_goods.id) of
      [] ->
        case cache_csv:get_commodity_config_without_exception(ShopItem#res_goods.id) of
          {success, _Com} -> {fail, "GoodsHasExist"};
          {fail, _ } ->
            ets:insert(shop_config_data, ShopItem),
            success
        end;
      [_Item] -> {fail, "GoodsHasExist"};
      _ ->
      {fail, "SystemError"}
  end,
  {reply, Ret, State};
handle_call({get_commodity_config, CommodityID}, _From, State) ->
  Ret =
    case ets:lookup(shop_config_data, CommodityID) of
      [] ->
        case cache_csv:get_commodity_config_without_exception(CommodityID) of
          {success, Com} -> {success, Com};
          {fail, _ } ->
            {success, DbNode} = dd_ms:read_config(database_node),
            case rpc:call(DbNode, database_monitor, execute, [query_shop_item_by_id, CommodityID]) of
              {success, Item} ->
                ets:insert(shop_config_data, Item),
                {success, Item};
              fail ->
                {fail, "SystemError"}
            end
        end;
      [ComItem] -> {success, ComItem};
      _ ->
        {fail, "SystemError"}
    end,
  {reply, Ret, State};
handle_call({del_commodity_config, ID}, _From, State) ->
  Ret =
    case ets:lookup(shop_config_data, ID) of
      [] -> {fail, "NotExist"};
      [_Config] ->
        ets:delete(shop_config_data, ID),
        success;
      Other ->
        ?FILE_LOG_ERROR("del commodity error, reason = ~p", [Other]),
        fail
    end,
  {reply, Ret, State};
handle_call({get_lottery_config_data, ID}, _From, State) ->
  Ret =
    case ets:lookup(lottery_config_data, ID) of
      [] ->
        case cache_csv:get_lottery_config_without_exception(ID) of
          {success, Con} ->
            ets:insert(lottery_config_data, Con),
            {success, Con};
          {fail, _ } ->
            {success, DbNode} = dd_ms:read_config(database_node),
            case rpc:call(DbNode, database_monitor, execute, [query_lottery_item_by_id, ID]) of
              {success, Item} ->
                ets:insert(lottery_config_data, Item),
                {success, Item};
              fail ->
                {fail, "SystemError"}
            end
        end;
      [Config] -> {success,Config};
      Other ->
        ?FILE_LOG_ERROR("del commodity error, reason = ~p", [Other]),
        throw({custom,"HintSystemDataError"})
    end,
  {reply, Ret, State};
handle_call(reload_lottery_config, _From, State) ->
  ets:delete_all_objects(lottery_config_data),
  LotteryItemList = load_lottery_config(),
  ?FILE_LOG_DEBUG("reload: LotteryItemList is ~p ",[LotteryItemList]),
  lists:foreach(
    fun(Item) ->
      ets:insert(lottery_config_data, Item)
    end, LotteryItemList),
  {reply, success, State};
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
handle_info({timeout, _TimerRef, 'start_load_config'}, State) ->
  ?FILE_LOG_DEBUG("START load mail config", []),
  ShopItemList = load_commodity_config_from_db(),
  lists:foreach(
    fun(Item) ->
      ets:insert(shop_config_data, Item)
    end, ShopItemList),
  ?FILE_LOG_DEBUG("FINISH load shop config", []),

  ?FILE_LOG_DEBUG("START load lottery config", []),
  LotteryItemList = load_lottery_config(),
  ?FILE_LOG_DEBUG("LotteryItemList is ~p ",[LotteryItemList]),
  lists:foreach(
    fun(Item) ->
      ets:insert(lottery_config_data, Item)
    end, LotteryItemList),
  ?FILE_LOG_DEBUG("FINISH load lottery config", []),
  {noreply, State};

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
load_commodity_config_from_db() ->
  {success, DbNode} = dd_ms:read_config(database_node),
  case rpc:call(DbNode, database_monitor, execute, [query_all_shop_item, 0]) of
    {success, GoodsList} -> GoodsList;
    Other ->
      ?FILE_LOG_DEBUG("load_good_config error, reason = ~p", [Other]),
      []
  end.

load_lottery_config() ->
  {success, DbNode} = dd_ms:read_config(database_node),
  case rpc:call(DbNode, database_monitor, execute, [query_all_lottery_item, 0]) of
    {success,[]} ->
      LotteryShopConfig1 = cache_csv:get_lottery_shop_config_by_id(1),
      LotteryShopConfig2 = cache_csv:get_lottery_shop_config_by_id(2),
      SingleL =  #lottery_conf{id = 1, is_discount = 0, price = LotteryShopConfig1#res_lottery_shop_config.price, tool_id = LotteryShopConfig1#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
      TenL = #lottery_conf{id = 2, is_discount = 0, price = LotteryShopConfig2#res_lottery_shop_config.price, tool_id = LotteryShopConfig2#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
      [SingleL,TenL];
    {success, GoodsList} ->                   %%GoodsList是数据库中的抽奖数据 ，数据格式为 lottery_conf记录 ,可能为1项纪录，可能为2项纪录
      case GoodsList of
        [OneLottery] ->                        %%单项纪录
          Type = OneLottery#lottery_conf.id,
          if
            Type =:= 1 ->  %%数据库中只有单抽打折信息，判断是否单抽打折，且十连抽为配置信息
              SingleLot =
                if                                                       % Account#account.lottery#lottery.single_lottery_times =< 0 ->
                  OneLottery#lottery_conf.is_discount =:= 1 -> %%打折
                    case check_lottery_time(OneLottery) of           %%检查抽奖打折时间
                      ok ->
                        SingleL = OneLottery,
                        SingleL;
                      _ ->
                        LotteryShopConfig1 = cache_csv:get_lottery_shop_config_by_id(1),
                        SingleL =  #lottery_conf{id = 1, is_discount = 0, price = LotteryShopConfig1#res_lottery_shop_config.price, tool_id =  LotteryShopConfig1#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
                        SingleL
                    end;
                  true ->
                    LotteryShopConfig1 = cache_csv:get_lottery_shop_config_by_id(1),
                    SingleL =  #lottery_conf{id = 1, is_discount = 0, price = LotteryShopConfig1#res_lottery_shop_config.price, tool_id =  LotteryShopConfig1#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
                    SingleL
                end,
              LotteryShopConfig2 = cache_csv:get_lottery_shop_config_by_id(2),
              TenLot = #lottery_conf{id = 2, is_discount = 0, price = LotteryShopConfig2#res_lottery_shop_config.price, tool_id = LotteryShopConfig2#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
              [SingleLot,TenLot];
            true ->      %%数据库中只有十连抽打折信息，判断是否十连抽打折，单抽为配置信息
              LotteryShopConfig1 = cache_csv:get_lottery_shop_config_by_id(1),
              SingleLot =  #lottery_conf{id = 1, is_discount = 0, price = LotteryShopConfig1#res_lottery_shop_config.price, tool_id =  LotteryShopConfig1#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
              TenLot =
                if
                  OneLottery#lottery_conf.is_discount =:= 1 -> %%打折
                    case check_lottery_time(OneLottery) of
                      ok ->
                        TenL = OneLottery,
                        TenL;
                      _ ->
                        LotteryShopConfig2 = cache_csv:get_lottery_shop_config_by_id(2),
                        TenL = #lottery_conf{id = 2, is_discount = 0, price = LotteryShopConfig2#res_lottery_shop_config.price, tool_id = LotteryShopConfig2#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
                        TenL
                    end;
                  true ->
                    LotteryShopConfig2 = cache_csv:get_lottery_shop_config_by_id(2),
                    TenL = #lottery_conf{id = 2, is_discount = 0, price = LotteryShopConfig2#res_lottery_shop_config.price, tool_id = LotteryShopConfig2#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
                    TenL
                end,
              [SingleLot,TenLot]
          end;
        [SingleLottery,TenLottery]->                        %%两条纪录
          SingleLot =
            if                                                       % Account#account.lottery#lottery.single_lottery_times =< 0 ->
              SingleLottery#lottery_conf.is_discount =:= 1 -> %%打折
                case check_lottery_time(SingleLottery) of           %%检查抽奖打折时间
                  ok ->
                    SingleL = SingleLottery,
                    SingleL;
                  _ ->
                    LotteryShopConfig1 = cache_csv:get_lottery_shop_config_by_id(1),
                    SingleL =  #lottery_conf{id = 1, is_discount = 0, price = LotteryShopConfig1#res_lottery_shop_config.price, tool_id = LotteryShopConfig1#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
                    SingleL
                end;
              true ->
                LotteryShopConfig1 = cache_csv:get_lottery_shop_config_by_id(1),
                SingleL =  #lottery_conf{id = 1, is_discount = 0, price = LotteryShopConfig1#res_lottery_shop_config.price, tool_id = LotteryShopConfig1#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
                SingleL
            end,
          TenLot =
            if
              TenLottery#lottery_conf.is_discount =:= 1 -> %%打折
                case check_lottery_time(TenLottery) of
                  ok ->
                    TenL = TenLottery,
                    TenL;
                  _ ->
                    LotteryShopConfig2 = cache_csv:get_lottery_shop_config_by_id(2),
                    TenL = #lottery_conf{id = 2, is_discount = 0, price = LotteryShopConfig2#res_lottery_shop_config.price, tool_id = LotteryShopConfig2#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
                    TenL
                end;
              true ->
                LotteryShopConfig2 = cache_csv:get_lottery_shop_config_by_id(2),
                TenL = #lottery_conf{id = 2, is_discount = 0, price = LotteryShopConfig2#res_lottery_shop_config.price, tool_id = LotteryShopConfig2#res_lottery_shop_config.sale_id,start_ts = 0,over_ts = 0},
                TenL
            end,
          [SingleLot,TenLot]
      end;
    Other ->
      ?FILE_LOG_DEBUG("load_lottery_config error, reason = ~p", [Other]),
      []
  end.

filter_commodity_out_of_date(CommodityList) ->
  filter_commodity_out_of_date_1(CommodityList, [], [], []).
filter_commodity_out_of_date_1([], UnStartList, OutOfDateList, OnGoingList) -> {UnStartList, OutOfDateList, OnGoingList};
filter_commodity_out_of_date_1([Item | T], UnStartList, OutOfDateList, OnGoingList) ->
  CurTs = dd_util:timestamp(),
  StartTs = Item#res_goods.start_ts,
  OverTs = Item#res_goods.over_ts,
  if
    StartTs =< 0 -> filter_commodity_out_of_date_1(T, UnStartList, OutOfDateList, [Item | OnGoingList]);
    StartTs <  CurTs andalso OverTs =:= 0 ->  filter_commodity_out_of_date_1(T, UnStartList, OutOfDateList, [Item | OnGoingList]);
    StartTs < CurTs andalso OverTs =< CurTs -> filter_commodity_out_of_date_1(T, UnStartList, [Item | OutOfDateList], OnGoingList);
    StartTs > CurTs -> filter_commodity_out_of_date_1(T, [Item | UnStartList], OutOfDateList, OnGoingList);
    true ->
      filter_commodity_out_of_date_1(T, UnStartList, OutOfDateList, [Item | OnGoingList])
  end.

check_lottery_time(Lottery) ->
  StartTs = Lottery#lottery_conf.start_ts,
  StopTs = Lottery#lottery_conf.over_ts,
  Now = dd_util:timestamp(),
  if
    StartTs < Now andalso StopTs > Now ->
      ok;
    true ->
      overtime
  end.


