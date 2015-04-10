%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 九月 2014 下午4:34
%%%-------------------------------------------------------------------
-module(cache_ranking).
-author("zqlt").

-behaviour(gen_server).
-include("../../deps/file_log/include/file_log.hrl").
-include("../ranking/ranking.hrl").
-include("cache_def.hrl").
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

-export([
  get_friend_tollgate_rank/3,
  get_friend_endless_rank/3,
  get_server_endless_rank/1
]).

-export([
  friend_tollgate_rank/1,
  friend_endless_rank/1,
  server_endless_rank/1
]).
-record(state, {}).



%%%===================================================================
%%% API
%%%===================================================================
get_friend_tollgate_rank(Uin, TollgateID, FriendUinList) ->
  gen_server:call(?MODULE, {event, {friend_tollgate_rank, {Uin, TollgateID, FriendUinList}}}).

get_friend_endless_rank(Uin, FriendUinList, EnergyHaveGiftUinList) ->
  gen_server:call(?MODULE, {event, {friend_endless_rank, {Uin, FriendUinList, EnergyHaveGiftUinList}}}).

get_server_endless_rank(Uin) ->
  gen_server:call(?MODULE, {event, {server_endless_rank, Uin}}).

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
  {success, RankingNode} = dd_ms:read_config(ranking_node),
  dd_config:write_cfg(ranking_node, RankingNode),
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
handle_call({event, {FuncName, FuncParam}}, _From, State) ->
  Ret = work_proc({execute, {FuncName, FuncParam}}),
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
terminate(Reason, _State) ->
  ?FILE_LOG_ERROR("cache_ranking stop, reason = ~p", [Reason]),
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
work_proc(Event) ->
  case Event of
    {execute, {FuncName, FuncParam}} ->
      try
        cache_ranking:FuncName(FuncParam)
      catch
        throw:{custom, Reason} ->
          ?FILE_LOG_ERROR("execute error, ~p, ~p, reason = ~p", [FuncName, FuncParam, Reason]),
          {fail, "HintSystemError"};
        What:Type ->
          ?FILE_LOG_ERROR("execute error, ~p, ~p, what = ~p, type = ~p, stack = ~p", [FuncName, FuncParam, What, Type]),
          {fail, "HintSystemError"}
      end;
    Other ->
      ?FILE_LOG_ERROR("error request, ~p", [Other])
  end.

friend_tollgate_rank({Uin, TollgateID, FriendUinList}) when is_integer(Uin) andalso is_integer(TollgateID) andalso is_list(FriendUinList) ->
  {success, RankNode} = dd_ms:read_config(ranking_node),
  case rpc:call(RankNode, ranking_tollgate, get_friend_tollgate_rank, [Uin, FriendUinList, TollgateID]) of
    {success, RankList} -> {success, RankList};
    Other ->
      ?FILE_LOG_ERROR("get friend tollgate rank error, reason = ~p, tollgateid = ~p", [Other, TollgateID]),
      {success, []}
  end.

%%0: 关闭，1 打开， 2 已经送过
friend_endless_rank({Uin, FriendUinList, EnergyHaveGiftUinList}) ->
  {success, RankNode} = dd_ms:read_config(ranking_node),
  {NoCoreList, LastRankList, ThisRankList} =
    case rpc:call(RankNode, ranking_tollgate, get_friend_endless_rank, [Uin, FriendUinList]) of
      {success, Value} -> Value;
      Other ->
        ?FILE_LOG_ERROR("get friend endless tollgate rank error, reason = ~p", [Other]),
        {[],[],[]}
    end,
  CloseGiftUinList = get_friend_strength_gift_info([Uin | FriendUinList]),
  %%添加到树结构中
  LastTree =
    lists:foldl(
      fun(Rank, TmpTree) ->
        case gb_trees:lookup(Rank#rank_info.uin, TmpTree) of
          none ->
            gb_trees:insert(Rank#rank_info.uin, {Rank, 1}, TmpTree);
          {value, _} -> TmpTree
        end
      end, gb_trees:empty(), LastRankList),
  ThisTree =
    lists:foldl(
      fun(Rank, TmpTree) ->
        case gb_trees:lookup(Rank#rank_info.uin, TmpTree) of
          none ->
            gb_trees:insert(Rank#rank_info.uin, {Rank, 1}, TmpTree);
          {value, _} -> TmpTree
        end
      end, gb_trees:empty(), ThisRankList),
  NoScoreTree =
    lists:foldl(
      fun(Rank, TmpTree) ->
        case gb_trees:lookup(Rank#rank_info.uin, TmpTree) of
          none ->
            gb_trees:insert(Rank#rank_info.uin, {Rank, 1}, TmpTree);
          {value, _} -> TmpTree
        end
      end, gb_trees:empty(), NoCoreList),
  %%更新赠送过的人数
  LastTree1 =
    lists:foldl(
      fun(GiftUin, TmpTree) ->
        case gb_trees:lookup(GiftUin, TmpTree) of
          none -> TmpTree;
          {value, {Rank, _}} -> gb_trees:update(GiftUin, {Rank, 2}, TmpTree)
        end
      end, LastTree, EnergyHaveGiftUinList),
  ThisTree1 =
    lists:foldl(
      fun(GiftUin, TmpTree) ->
        case gb_trees:lookup(GiftUin, TmpTree) of
          none -> TmpTree;
          {value, {Rank, _}} -> gb_trees:update(GiftUin, {Rank, 2}, TmpTree)
        end
      end, ThisTree, EnergyHaveGiftUinList),
  NoScoreTree1 =
    lists:foldl(
      fun(GiftUin, TmpTree) ->
        case gb_trees:lookup(GiftUin, TmpTree) of
          none -> TmpTree;
          {value, {Rank, _}} -> gb_trees:update(GiftUin, {Rank, 2}, TmpTree)
        end
      end, NoScoreTree, EnergyHaveGiftUinList),
  %%更新已关闭列表
  LastTree2 =
    lists:foldl(
      fun(CloseUin, TmpTree) ->
        case gb_trees:lookup(CloseUin, TmpTree) of
          none -> TmpTree;
          {value, {Rank, _}} -> gb_trees:update(CloseUin, {Rank, 0}, TmpTree)
        end
      end, LastTree1, CloseGiftUinList),
  ThisTree2 =
    lists:foldl(
      fun(CloseUin, TmpTree) ->
        case gb_trees:lookup(CloseUin, TmpTree) of
          none -> TmpTree;
          {value, {Rank, _}} -> gb_trees:update(CloseUin, {Rank, 0}, TmpTree)
        end
      end, ThisTree1, CloseGiftUinList),
  NoScoreTree2 =
    lists:foldl(
      fun(CloseUin, TmpTree) ->
        case gb_trees:lookup(CloseUin, TmpTree) of
          none -> TmpTree;
          {value, {Rank, _}} -> gb_trees:update(CloseUin, {Rank, 0}, TmpTree)
        end
      end, NoScoreTree1, CloseGiftUinList),

  {success, {gb_trees:values(NoScoreTree2), gb_trees:values(LastTree2), gb_trees:values(ThisTree2)}}.

server_endless_rank(Uin) ->
  {success, RankNode} = dd_ms:read_config(ranking_node),
  case rpc:call(RankNode, ranking_endless, get_endless_rank, [Uin]) of
    {success, Value} -> {success, Value};
    Other ->
      ?FILE_LOG_DEBUG("get server endless rank error, reason = ~p", [Other]),
      {fail, "HintSystemError"}
  end.

 %%0: 关闭，1 打开， 2 已经送过
get_friend_strength_gift_info([]) -> [];
get_friend_strength_gift_info(UinList) when is_list(UinList)  ->
  {success, DBNode} = dd_ms:read_config(database_node),
  case rpc:call(DBNode, database_monitor, execute, [get_energy_gift_state, UinList]) of
    {success, StateList} -> StateList;
    Other ->
      ?FILE_LOG_DEBUG("get_energy_gift_state error, reason = ~p", [Other]),
      []
  end.

