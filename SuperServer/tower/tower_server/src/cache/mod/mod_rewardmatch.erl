-module(mod_rewardmatch).
-author("zqlt").

%% API
-export([execute/2]).

-include("../../../deps/file_log/include/file_log.hrl").
-include("../../mail/mail.hrl").
-include("../../ranking/ranking.hrl").
-include("../../csv.hrl").
-include("../../dd_ms.hrl").
-include("../cache_def.hrl").
-include("../cache_cdkey.hrl").

-define(MAX_TEAM_MEMBER,3).

%赏金赛buff类型，特殊
-define(RM_BT_S_MULT, 1).%多点选择
-define(RM_BT_S_NOBUFF, 4).%一张白纸
-define(RM_BT_S_DOCTOR, 8).%白衣天使
-define(RM_BT_S_DIFFICULT, 16).%喜欢挑战
-define(RM_BT_S_RANDOM, 32).%爱谁谁

%赏金赛buff类型，翻卡时
-define(RM_BT_REF_LOWLEVEL, 1).%低水平较量
-define(RM_BT_REF_TRASH, 2).%倒垃圾
-define(RM_BT_REF_LOTTERY, 4).%中彩票
-define(RM_BT_REF_BOSSDEAD, 8).%老板去死
-define(RM_BT_REF_HALFFEE, 16).%口袋破了

-define(RM_BT_REF_DOUBLE_BOX, 32).%土豪
-define(RM_BT_REF_DOUBLE_BUFF, 64).%状态来了
-define(RM_BT_REF_DOUBLE_ELITE, 128).%高水平较量

-define(RM_BT_REF_NOBOX, 256).%穷光蛋

-define(RM_BT_REF_NOREFRESH, 512).%不想刷新
-define(RM_BT_REF_DOCTOR, 1024).%不想刷新


%赏金赛buff类型，赢
-define(RM_BT_WIN_RECOVER, 1).%霸气十足
-define(RM_BT_WIN_NORECOVER, 2).%霸气漏光
-define(RM_BT_WIN_FEE, 4).%守财奴
-define(RM_BT_WIN_COMBO, 8).%机关枪

%赏金赛buff类型，输
-define(RM_BT_LOSE_WILLFUL, 1).%就是任性

%获取信息
execute("get_info", {Account, _Params}) ->
	%赏金赛队伍不齐的话，复制常规赛队伍
	NeedInit = lists:any(fun(Player) -> Player =:= {0,0} end, 
						 Account#account.reward_match#reward_match.reward_team),
	
	if	
		NeedInit =:= true -> 
			NReward_match = Account#account.reward_match#reward_match{reward_team = Account#account.players#players.select_players},
			NAccount = Account#account{reward_match = NReward_match},
			{NAccount, ["reward_match"], NReward_match };
		true -> 
			{Account, ["reward_match"], Account#account.reward_match }
	end;

%入场（使用券）
execute("enter", {Account, _Params}) ->
	{struct, [{<<"paramlist">>, [Level]}]} = mochijson2:decode(_Params),
	NAccount = use_ticket(Account),
	RewardMatch = enter(NAccount#account.reward_match,Level),
	NNAccount = NAccount#account{reward_match = RewardMatch},
	{NNAccount,["reward_match","reward_match_ts"],RewardMatch};

%离场（主动触发）
execute("leave", {Account, _Params}) ->
	RewardMatch = leave(Account#account.reward_match),
	NNAccount = Account#account{reward_match = RewardMatch},
	{NNAccount,["reward_match"],RewardMatch};

execute("select", {Account, _Params})->
	{struct, [{<<"paramlist">>, [Index]}]} = mochijson2:decode(_Params),
	RewardMatch = select_card(Account#account.reward_match,Index),
    NAccount = Account#account{reward_match = RewardMatch},
  	{NAccount,["reward_match"],RewardMatch};

execute("battle_result", {Account, _Params}) ->
	{struct, [{<<"paramlist">>, [BattleInfo]}]} = mochijson2:decode(_Params),
  	RewardMatch = endbattle(Account#account.reward_match,BattleInfo),
	NAccount = Account#account{reward_match = RewardMatch},
	{NAccount,["reward_match"],RewardMatch};

execute("fee_change", {Account, _Params}) ->
	{struct, [{<<"paramlist">>, [Count]}]} = mochijson2:decode(_Params),
  	RewardMatch = fee_change(Account#account.reward_match,Count),
	NAccount = Account#account{reward_match = RewardMatch},
	{NAccount,["reward_match"],RewardMatch};


%%设置赏金队伍
execute("change_reward_team",{Account,Params})->
	RewardTeam = gateway_util:decode_reward_team_json(Params),
	%%校验下队伍的基本情况
  	check_reward_team(Account, RewardTeam),
  	%%替换队伍列表
  	NTAccount= update_reward_team(Account, RewardTeam),
	{NTAccount,["reward_match"],NTAccount#account.reward_match};

execute("add_ticket", {Account, _Params}) ->
	RMTS = Account#account.reward_match_ts#ts_item{count = 100},
	NAccount = Account#account{reward_match_ts = RMTS},
  	{NAccount, ["reward_match_ts"], NAccount#account.reward_match}.



%状态(入场)
statu_enter(RewardMatch,Level)  when is_record(RewardMatch, reward_match) ->
	RewardMatch#reward_match{
								level = Level,
								status = 1,
								fee_temp = 0,
								fee_battle = 0,
								combo = 0,
								dis_slot = 0,
								hp = 300,
								battle_id = 0,
								select_index = 0,
								max_index = 0,
								card_list=[],
								card_list_hid=[],
								buff_list=[]}.
	
%状态(离场)
statu_leave(RewardMatch) when is_record(RewardMatch, reward_match) ->
	Fee = RewardMatch#reward_match.fee,
	Feetemp = RewardMatch#reward_match.fee_temp,
	RewardMatch#reward_match{
								fee = Fee + Feetemp,
								fee_temp = 0,
								fee_battle = 0,
								status = 0,
								combo = 0,
								dis_slot = 0,
								hp = 0,
								battle_id = 0,
								select_index = 0,
								max_index = 0,
								card_list=[],
								card_list_hid=[],
								buff_list=[]}.

%状态（战斗）
statu_battle_on(RewardMatch,BID)->
	RewardMatch#reward_match{status = 2,battle_id = BID,fee_battle = 0}.

%状态（战斗完成）
statu_battle_off(RewardMatch)->
	RewardMatch#reward_match{status = 1}.

%改变赏金币（使用，获取）
fee_change(RewardMatch,Count) when is_record(RewardMatch, reward_match) ->
	NFee = RewardMatch#reward_match.fee + Count,
    if
		NFee < 0 -> throw({custom, "HintNotEnoughFee"});
		true -> RewardMatch#reward_match{fee = NFee}
	end.

%使用入场券
use_ticket(Account)  when is_record(Account, account) ->
	%校验状态
	cache_api:check_status(Account#account.reward_match#reward_match.status,?RM_STATUS_CLOSE),
	%校验入场券
    ?FILE_LOG_DEBUG("enter check count = ~p", [Account#account.reward_match_ts#ts_item.count]),
  	cache_api:check_count(1,Account#account.reward_match_ts#ts_item.count),
	
	Ts_info = cache_api:dec_ts_count(Account#account.reward_match_ts,1,"enter"),
	?FILE_LOG_DEBUG("enter check ts_info = ~p", [Ts_info]),
  	Account#account{reward_match_ts = Ts_info}.	

%更新历史场次
update_levelcount(RewardMatch,Level)  when is_record(RewardMatch, reward_match) ->
  CurComboMax = lists:keyfind(Level, 2, RewardMatch#reward_match.levelcount),
  NComboMaxList = 
  case CurComboMax of
		{rm_combo,CLevel,CMax}-> 
			lists:keyreplace(Level, 2, RewardMatch#reward_match.levelcount, {rm_combo,CLevel,CMax + 1});
		false-> 
			lists:append(RewardMatch#reward_match.levelcount, [#rm_combo{level=Level,max=1}])
  end,
  RewardMatch#reward_match{levelcount = NComboMaxList}.


%入场
enter(RewardMatch,Level) when is_record(RewardMatch, reward_match) ->
  %校验最大开启等级
  ?FILE_LOG_DEBUG("enter check 33333 ~p", [RewardMatch#reward_match.levelmax]),
  cache_api:check_count(Level,RewardMatch#reward_match.levelmax),
  RewardMatch_1 = statu_enter(RewardMatch,Level),
  RewardMatch_2 = update_levelcount(RewardMatch_1,Level),
  refresh_card(RewardMatch_2).

%离场（失败过多，或者胜12场，或者主动退出）
leave(RewardMatch) when is_record(RewardMatch, reward_match) ->
	statu_leave(RewardMatch).

%选择卡片
select_card(RewardMatch,Index) ->
	%按目前逻辑，战斗后杀进程，算没有打，则选择卡牌时不做是否战斗中的校验
	%cache_api:check_status(RewardMatch#reward_match.status,?RM_STATUS_OPEN),
	%此处结构为{rm_card,1,2,1000}，取第二个元素为index，第三个元素为type
	SelectCard = lists:keyfind(Index, 2, RewardMatch#reward_match.card_list),
	?FILE_LOG_DEBUG("select_card ~p", [RewardMatch#reward_match.card_list]),
	
	case SelectCard of
		false -> 
			?FILE_LOG_ERROR("erro card Index, [~p]", [Index]),
			throw({custom, "HintRMCardIndex"});
		CardRecord	->	
			RewardMatch_1 = RewardMatch#reward_match{select_index = Index},
			open_card(RewardMatch_1,CardRecord)
	end.
	

open_card(RewardMatch,{rm_card,_CardIndex,CardType,CardID}) ->
	case CardType of
		0 -> NRewardMatch = openbox(RewardMatch),
			 refresh_card(NRewardMatch);
		1 -> {NRewardMatch,F_R} =
			if
				CardID =:= 3 ->
					RewardMatch_1 = RewardMatch#reward_match{buff_list = format_bufflist_doctor(RewardMatch)},
					openbuff(RewardMatch_1,CardID);
				true -> 
					openbuff(RewardMatch,CardID)
			end,
			 refresh_card_buff(NRewardMatch,F_R);
		2 -> openbattle(RewardMatch,CardID)
	end.

%开宝箱
openbox(RewardMatch) when is_record(RewardMatch, reward_match) ->
	FeeTemp = RewardMatch#reward_match.fee_temp,
	RewardMatch#reward_match{fee_temp = FeeTemp + ?BOX_FEE}.

%开buff
openbuff(RewardMatch,ID) when is_record(RewardMatch, reward_match) ->
	%判断是否是爱谁谁
	NID = 
	if
		ID =:= 0 -> dd_util:random_in_range(19);
		true -> ID
	end,
		
	Rm_buff_data = cache_csv:get_rm_buff_info(NID),
	
	NRewardMatch = 
	case Rm_buff_data#rm_buff_data.type of
		1-> Buff = #rm_buff{index = RewardMatch#reward_match.max_index_buff, id=Rm_buff_data#rm_buff_data.id,type=1},
			Bufflist = RewardMatch#reward_match.buff_list,
			NBufflist = cache_api:insert_length_list(Buff,Bufflist,?BUFF_SLOT),
			RewardMatch#reward_match{buff_list = NBufflist,max_index_buff = RewardMatch#reward_match.max_index_buff + 1};
		_-> RewardMatch
	end,
	{NRewardMatch,Rm_buff_data#rm_buff_data.flagRefresh}.
	

%开战斗
openbattle(RewardMatch,BID) when is_record(RewardMatch, reward_match) ->
	%cache_api:check_status(RewardMatch#reward_match.status,?RM_STATUS_OPEN),
	statu_battle_on(RewardMatch,BID).

%结束战斗
endbattle(RewardMatch,BattleInfo) when is_record(RewardMatch, reward_match) ->
	cache_api:check_status(RewardMatch#reward_match.status,?RM_STATUS_BATTLE),
	NRewardMatch = statu_battle_off(RewardMatch),
	case BattleInfo of
		0->lose(NRewardMatch);
		1->win(NRewardMatch)
	end.	
	
%战斗胜利
win(RewardMatch)  when is_record(RewardMatch, reward_match) ->
	%判断机关枪buff
	ComboAdd = 
	lists:foldl(fun(Buffer, Sum) -> 
						Rm_buff_data = cache_csv:get_rm_buff_info(Buffer#rm_buff.id),
					 	case Rm_buff_data#rm_buff_data.flagWin of
							?RM_BT_WIN_COMBO -> Sum + 1;
							_ -> Sum
					 	end
				end, 1, RewardMatch#reward_match.buff_list),
	
	Combo = RewardMatch#reward_match.combo + ComboAdd,
	Level = RewardMatch#reward_match.level,
	CurComboMax = lists:keyfind(Level, 2, RewardMatch#reward_match.combomax),
	
	?FILE_LOG_DEBUG("win reward_match = ~p", [RewardMatch]),
	
	NComboMaxList = 
	case CurComboMax of
		{rm_combo,CLevel,CMax}-> if
									 Combo > CMax -> lists:keyreplace(Level, 2, RewardMatch#reward_match.combomax, {rm_combo,CLevel,Combo});
									 true -> RewardMatch#reward_match.combomax
								 end;
		false-> lists:append(RewardMatch#reward_match.combomax, [#rm_combo{level=Level,max=Combo}])
	end,

	NRewardMatch = RewardMatch#reward_match{combo = Combo,combomax=NComboMaxList },
	?FILE_LOG_DEBUG("win combomax = ~p", [RewardMatch#reward_match.combomax]),
	Con1 = Combo =:= 10,
	Con2 = NRewardMatch#reward_match.levelmax < 3,
	Con3 = NRewardMatch#reward_match.level =:= NRewardMatch#reward_match.levelmax,
	Con4 = Con1 and Con2,
	Con5 = Con4 and Con3,
						
	NNRewardMatch = 
	case Con5 of
		true -> NLevelmax = NRewardMatch#reward_match.levelmax + 1,
				NRewardMatch#reward_match{levelmax = NLevelmax};
		false -> NRewardMatch
	end,
						
	%计算斗气回复
    DoNoRecver = is_debuff_effect(f_w,?RM_BT_WIN_NORECOVER,RewardMatch),
					  
	RewardMatch_3 = 
	if
		DoNoRecver =:= true-> NNRewardMatch;
		true -> 
			%Has_Recover= has_buff(f_w,?RM_BT_WIN_RECOVER,NRewardMatch),
			Has_Recover = is_buff_effect(f_w,?RM_BT_WIN_RECOVER,NRewardMatch),
			HPMax = NNRewardMatch#reward_match.hpmax,
			if
				Has_Recover =:= true ->	
					NNRewardMatch#reward_match{hp = HPMax};
				true -> 
					HpAdd = 	
					case NNRewardMatch#reward_match.level of
						0-> (HPMax * 40) div 100;
						1->	(HPMax * 30) div 100;
						2->	(HPMax * 20) div 100;
						3->	(HPMax * 10) div 100;
						_-> 0
					end,
					HpNext = NNRewardMatch#reward_match.hp + HpAdd,
					HpNext_1 = 
					if
						HpNext > HPMax -> HPMax;
						true -> HpNext
					end,
					NNRewardMatch#reward_match{hp = HpNext_1}
			end
	end,
	
	WIN_FEE_Count = has_buff_count(f_w,?RM_BT_WIN_FEE,RewardMatch_3),
	
	%计算奖励
	Rm_battle_data = cache_csv:get_rm_battle_info(RewardMatch#reward_match.battle_id),
	FeeAdd = Rm_battle_data#rm_battle_data.fee + (Rm_battle_data#rm_battle_data.fee * WIN_FEE_Count) div 5,
	FeeCur = RewardMatch_3#reward_match.fee_temp,
	RewardMatch_4 = RewardMatch_3#reward_match{fee_temp = FeeCur + FeeAdd,fee_battle = FeeAdd},
	
	if
		Combo < 12 -> refresh_card(RewardMatch_4);
		true-> leave(RewardMatch_4)
	end.

%战斗失败
lose(RewardMatch)  when is_record(RewardMatch, reward_match) ->
	Disslot = RewardMatch#reward_match.dis_slot + 1,
	IsMutliBuff = fun(RM_BUFF) -> RM_BUFF#rm_buff.id /= ?ID_BUFF_MULTI end,
	NBufflist = lists:filter(IsMutliBuff, RewardMatch#reward_match.buff_list),%清理多点选择buff
	HPMax = RewardMatch#reward_match.hpmax,
	NRewardMatch = RewardMatch#reward_match{
								status = 1, 
								dis_slot = Disslot,
								hp = HPMax,
								card_list=[],
								buff_list=NBufflist},
	?FILE_LOG_DEBUG("lose NRewardMatch = ~p", [NRewardMatch]),
	
	%判断任性	
	Rm_battle_data = cache_csv:get_rm_battle_info(RewardMatch#reward_match.battle_id),
	FeeDel = Rm_battle_data#rm_battle_data.feelose,
	FeeCur = NRewardMatch#reward_match.fee_temp,
	FeeNext = FeeCur - FeeDel,
	FeeNext_1 = 
	if
		FeeNext < 0 -> 0;
		true -> FeeNext
	end,
	
	%Has_Recover= has_buff(f_w,?RM_BT_LOSE_WILLFUL,NRewardMatch),
	Has_Recover = is_buff_effect(f_w,?RM_BT_LOSE_WILLFUL,NRewardMatch),
	
	RewardMatch_2 = 
	if
		Has_Recover =:= true -> NRewardMatch;
		true -> NRewardMatch#reward_match{fee_temp = FeeNext_1,fee_battle = 0 - FeeDel,combo = 0}
	end,
	
	if
		Disslot < 3 -> refresh_card(RewardMatch_2);
		true -> ?FILE_LOG_DEBUG("lose leave NRewardMatch = ~p", [RewardMatch_2]),
				leave(RewardMatch_2)
	end.

%判断当前是否拥有指定效果的buff（true,false）
has_buff(FlagType,Flag,RewardMatch)->
	lists:any(fun(Buffer) -> 
				Rm_buff_data = cache_csv:get_rm_buff_info(Buffer#rm_buff.id),
				case FlagType of
					f_r->Rm_buff_data#rm_buff_data.flagRefresh =:= Flag;
					f_w->Rm_buff_data#rm_buff_data.flagWin =:= Flag;
					f_l->Rm_buff_data#rm_buff_data.flagLose =:= Flag;
					_->false
				end
			  end, RewardMatch#reward_match.buff_list).

%判断当前是否拥有指定效果的buff 数量
has_buff_count(FlagType,Flag,RewardMatch)->
	lists:foldl(fun(Buffer, Sum) ->
					 Rm_buff_data = cache_csv:get_rm_buff_info(Buffer#rm_buff.id),
					 case	FlagType of
						f_r->
							if
								Rm_buff_data#rm_buff_data.flagRefresh =:= Flag ->Sum+1;
								true->Sum
							end;
						f_w->
							if
								Rm_buff_data#rm_buff_data.flagWin =:= Flag ->Sum+1;
								true->Sum
							end;
						f_l->
							if
								Rm_buff_data#rm_buff_data.flagLose =:= Flag ->Sum+1;
								true->Sum
							end;
						_ ->Sum
					 end
				end, 0, RewardMatch#reward_match.buff_list).

%判断当前是否拥有指定id buff
has_buffid(BID,RewardMatch) ->
	lists:any(fun(Buffer) -> Buffer#rm_buff.id =:= BID end, RewardMatch#reward_match.buff_list).

%判断即时debuff是否可生效（有无白衣天使）
is_debuff_effect_im(Flag,FlagDef,RewardMatch) -> 
	IsSame = Flag=:=FlagDef,
	Has_Doctor = has_buffid(3,RewardMatch),
	IsSame and (not Has_Doctor).

%判断debuff是否可生效（有无白衣天使）
is_debuff_effect(FlagType,Flag,RewardMatch)->
	Has_Debuff = has_buff(FlagType,Flag,RewardMatch),
	Has_Doctor = has_buffid(3,RewardMatch),
	Has_Debuff and (not Has_Doctor).

%判断buff是否可生效（有无白衣天使,有无一张白纸）
is_buff_effect(FlagType,Flag,RewardMatch)->
	Has_Debuff = has_buff(FlagType,Flag,RewardMatch),
	Has_Doctor = has_buffid(3,RewardMatch),%白衣天使
	Has_White = has_buffid(2,RewardMatch),%一张白纸
	EX_Condition = Has_Doctor or (not Has_White),
	Has_Debuff and EX_Condition.
	

%获取需要保留的卡
get_remain_list(RewardMatch,RemainList)  when is_record(RewardMatch, reward_match) ->
	Predicate = fun(Card) -> 
						FindFun = fun(CardRemain) -> Card#rm_card.index =:= CardRemain#rm_card.index end,
						lists:any(FindFun, RemainList)
				end,
	lists:filter(Predicate, RewardMatch#reward_match.card_list).

%获取选中的卡
get_select_list(RewardMatch)  when is_record(RewardMatch, reward_match) ->
	Predicate = fun(Card) -> 
						if
							Card#rm_card.index =:= RewardMatch#reward_match.select_index -> true;
							true->false
						end
				end,
	lists:filter(Predicate, RewardMatch#reward_match.card_list).

%获取非选中的卡
get_no_select_list(RewardMatch)  when is_record(RewardMatch, reward_match) ->
	Predicate = fun(Card) -> 
						if
							Card#rm_card.index /= RewardMatch#reward_match.select_index -> true;
							true->false
						end
				end,
	
	%CardList = lists:filter(Predicate, RewardMatch#reward_match.card_list),
	%{CardList_1,_LastIndex} = lists:mapfoldr(fun(Card, Index) -> {Card#rm_card{index=Index}, Index + 1} end, 1, CardList),
	%?FILE_LOG_DEBUG("get_boss_list NNBossList = ~p", [CardList_1]),
	%CardList_1.
	%新逻辑中，索引不变
	lists:filter(Predicate, RewardMatch#reward_match.card_list).

%获取非选中的boss卡
get_boss_list(RewardMatch)  when is_record(RewardMatch, reward_match) ->
	Predicate = fun(Card) -> 
						if
							Card#rm_card.type == 2 -> 
								if
									Card#rm_card.index /=  RewardMatch#reward_match.select_index ->
										Rm_battle_data = cache_csv:get_rm_battle_info(Card#rm_card.id),
										if
											Rm_battle_data#rm_battle_data.type =:= 2  -> true;
											Rm_battle_data#rm_battle_data.type =:= 3  -> true;
											true -> false
										end;
									true->false
								end;
							true -> false
						end
				end,
	
	%NBossList = lists:filter(Predicate, RewardMatch#reward_match.card_list),
	%{NNBossList,_LastIndex} = lists:mapfoldr(fun(Card, Index) -> {Card#rm_card{index=Index}, Index + 1} end, 1, NBossList),
	%?FILE_LOG_DEBUG("get_boss_list NNBossList = ~p", [NNBossList]),
	%NNBossList.
	%新逻辑中，索引不变
	lists:filter(Predicate, RewardMatch#reward_match.card_list).

%获取可见卡孔数量
get_slot_count(RewardMatch) when is_record(RewardMatch, reward_match) ->
	SlotEXCount = lists:foldl(fun(Buffer, Sum) 
					 -> case	Buffer#rm_buff.id of
							?ID_BUFF_MULTI -> Sum + 1;
							_ -> Sum
						end
				end, 0, RewardMatch#reward_match.buff_list),
	DISSLOT = RewardMatch#reward_match.dis_slot,
	?FILE_LOG_DEBUG("refresh_card SlotEXCount = ~p DISSLOT = ~p", [SlotEXCount,DISSLOT]),
	?DEFAULT_SLOT + SlotEXCount - DISSLOT.

%获取可见卡组索引(不包括保留的boss卡及倒垃圾卡)
get_slot_list(RewardMatch,BeginIndex) when is_record(RewardMatch, reward_match) ->
	lists:seq(BeginIndex+1, get_slot_count(RewardMatch)).

get_slot_list_with_count(RewardMatch,Count) when is_record(RewardMatch, reward_match) ->
	BeginIndex = RewardMatch#reward_match.max_index,
	lists:seq(BeginIndex+1, BeginIndex + Count).
	
%生成宝箱卡（随机）
make_box_card(Index)->
	%BUFFID = dd_util:random_in_range(0,19),
	#rm_card{index=Index,type = 0,id=0}.

no_debuff(MaxID)->
	BUFFID = dd_util:random_in_range(0,MaxID),
	Rm_buff_data = cache_csv:get_rm_buff_info(BUFFID),
	if
		Rm_buff_data#rm_buff_data.isdebuff =:= 1 -> no_debuff(MaxID);
		true->BUFFID
	end.

%生成buff卡（随机）
make_buff_card(Index,RewardMatch)->
	Allcount = length(rm_buff_data:get_indexs()),
	BUFFID = dd_util:random_in_range(0,Allcount-1),
	Rm_buff_data = cache_csv:get_rm_buff_info(BUFFID),
	
	NBUFFID =
	if
		Rm_buff_data#rm_buff_data.isdebuff =:= 1 ->
			Has_Doctor = has_buffid(3,RewardMatch),
			if	
				Has_Doctor =:= true ->%no_debuff(Allcount-1);
					cache_csv:get_rm_buff_random_id(0);
				true ->
					BUFFID
			end;
		true->BUFFID
	end,
	#rm_card{index=Index,type = 1,id=NBUFFID}.
%% 	%TODO：测试用用完记得注销
%% 	BUFFID_test = dd_util:random_in_range(0,30),
%% 	if
%% 		BUFFID_test < 20 -> #rm_card{index=Index,type = 1,id=BUFFID_test};
%% 		true -> #rm_card{index=Index,type = 1,id=1}
%% 	end.

%生成战斗卡（根据几率）
make_battle_card(Index,{R_BOSS,R_ELITE,R_MONSTER}) ->
	RanBattle = dd_util:random_in_range(R_BOSS + R_ELITE + R_MONSTER),
  	if
		RanBattle =< R_BOSS ->%#rm_card{index=Index,type = 2,id=1};
      BossRate = dd_util:random_in_range(3),
      if
        BossRate < 2 -> %#rm_card{index=Index,type = 2,id=3};
          #rm_card{index=Index,type = 2,id=cache_csv:get_rm_battle_random_id(2)};
        true -> %#rm_card{index=Index,type = 2,id=4}
          #rm_card{index=Index,type = 2,id=cache_csv:get_rm_battle_random_id(3)}
      end;
		RanBattle =< R_BOSS + R_ELITE ->%#rm_card{index=Index,type = 2,id=2}; 
				#rm_card{index=Index,type = 2,id=cache_csv:get_rm_battle_random_id(1)};
		true ->
        #rm_card{index=Index,type = 2,id=cache_csv:get_rm_battle_random_id(0)}
	end.

%根据权重生成卡组(老逻辑，可能废弃)
make_card_with_rate(RewardMatch,HeadCardList,{NNR_BOX,NR_BUFF,NR_BATTLE},{NR_BOSS,NR_ELITE,NR_MONSTER}) when is_record(RewardMatch, reward_match)->
	Cardlist = get_slot_list(RewardMatch,length(HeadCardList)),
	NCardlist = 
	lists:map(
	  fun(X) -> 
			Ran = dd_util:random_in_range(NNR_BOX + NR_BUFF + NR_BATTLE),
	  		if
			  Ran =< NNR_BOX ->make_box_card(X);  
			  Ran =< NNR_BOX + NR_BUFF -> make_buff_card(X,RewardMatch); 
			  true -> make_battle_card(X,{NR_BOSS,NR_ELITE,NR_MONSTER})
			end
	  end, Cardlist),
	lists:append(HeadCardList,NCardlist).

%根据权重生成隐藏卡组
make_tempcard_with_rate(RewardMatch,Count,{NNR_BOX,NR_BUFF,NR_BATTLE},{NR_BOSS,NR_ELITE,NR_MONSTER}) when is_record(RewardMatch, reward_match)->
	Cardlist = get_slot_list_with_count(RewardMatch,Count),
	lists:map(
	  fun(X) -> 
			Ran = dd_util:random_in_range(NNR_BOX + NR_BUFF + NR_BATTLE),
	  		if
			  Ran =< NNR_BOX ->make_box_card(X);  
			  Ran =< NNR_BOX + NR_BUFF -> make_buff_card(X,RewardMatch); 
			  true -> make_battle_card(X,{NR_BOSS,NR_ELITE,NR_MONSTER})
			end
	  end, Cardlist).

n_make_hidcard_list(RewardMatch,{R_BOX,R_BUFF,R_BATTLE},{R_BOSS,R_ELITE,R_MONSTER}) when is_record(RewardMatch, reward_match)->
	Count = ?DEFAULT_HID_SLOT + ?DEFAULT_SLOT
			-length(RewardMatch#reward_match.card_list)
			-length(RewardMatch#reward_match.card_list_hid),
	HidButtonCard  = make_tempcard_with_rate(RewardMatch,Count
							,{R_BOX,R_BUFF,R_BATTLE}
							,{R_BOSS,R_ELITE,R_MONSTER}),
	HidList_1 = lists:append(RewardMatch#reward_match.card_list_hid,HidButtonCard),
	TMaxIndex = RewardMatch#reward_match.max_index,
	RewardMatch#reward_match{card_list_hid = HidList_1, max_index = TMaxIndex + Count}.
  
%生成卡组（判断是否保留boss卡，根据几率）
make_card_list(RewardMatch,{R_BOX,R_BUFF,R_BATTLE},{R_BOSS,R_ELITE,R_MONSTER},BossRemain,TrashRemain)  when is_record(RewardMatch, reward_match) ->
	%刷卡几率buff计算
	{NR_BOX,NR_BUFF,NR_BATTLE} = 
	lists:foldl(fun(Buffer, {TR_BOX,TR_BUFF,TR_BATTLE}) ->
					 Rm_buff_data = cache_csv:get_rm_buff_info(Buffer#rm_buff.id),
					 case	Rm_buff_data#rm_buff_data.flagRefresh of
						?RM_BT_REF_DOUBLE_BOX -> {TR_BOX+R_BOX,TR_BUFF,TR_BATTLE};
						?RM_BT_REF_DOUBLE_BUFF ->{TR_BOX,TR_BUFF+R_BUFF,TR_BATTLE};
						_ -> {TR_BOX,TR_BUFF,TR_BATTLE}
					 end
				end, {R_BOX,R_BUFF,R_BATTLE}, RewardMatch#reward_match.buff_list),
	
	%刷怪几率buff计算
	{NR_BOSS,NR_ELITE,NR_MONSTER} = 
	lists:foldl(fun(Buffer, {TR_BOOS,TR_ELITE,TR_MONSTER}) ->
					 Rm_buff_data = cache_csv:get_rm_buff_info(Buffer#rm_buff.id),
					 case	Rm_buff_data#rm_buff_data.flagRefresh of
						?RM_BT_REF_DOUBLE_ELITE -> {TR_BOOS,TR_ELITE+R_ELITE,TR_MONSTER};
						_ -> {TR_BOOS,TR_ELITE,TR_MONSTER}
					 end
				end, {R_BOSS,R_ELITE,R_MONSTER}, RewardMatch#reward_match.buff_list),
	
	%高优先级判断buff（穷光蛋）
	Do_Poor = is_debuff_effect(f_r,?RM_BT_REF_NOBOX,RewardMatch),
	NNR_BOX = 
	if
		Do_Poor =:= true -> 0;
		true -> NR_BOX
	end,

	%NoFresh = has_buff(f_r,?RM_BT_REF_NOREFRESH,RewardMatch),
	NoFresh = is_buff_effect(f_r,?RM_BT_REF_NOREFRESH,RewardMatch),
	
	HeadCardList_1 = 
	if
		NoFresh=:= true->
			get_no_select_list(RewardMatch);
		true->
			BossCardList =
			if 
				BossRemain =:= true -> get_boss_list(RewardMatch);
				true -> []
			end,
	
			TrashCardList= 
			if 
				%TODO目前用ID来生成倒垃圾卡，如果变换ID需要修改
				TrashRemain =:= true -> get_select_list(RewardMatch);
				true -> []
			end,
			lists:append(TrashCardList,BossCardList)
	end,
	
	HeadCardList = get_remain_list(RewardMatch,HeadCardList_1),

	CountHeadCardList = length(HeadCardList),
	CountSee = get_slot_count(RewardMatch),
	CountHid = length(RewardMatch#reward_match.card_list_hid),
	
	RewardMatch_1 = 
	if	
		CountHeadCardList < CountSee ->
			CountMove = CountSee - CountHeadCardList,
			if
				CountHid >= CountMove -> 
					{TempMovelist,TempHidList} = lists:split(CountMove, RewardMatch#reward_match.card_list_hid),
					TempCardList = lists:append(HeadCardList,TempMovelist),
					RewardMatch#reward_match{card_list = TempCardList,card_list_hid = TempHidList};
				true-> 
					CountRefresh = CountMove - CountHid,
					CardListAdd = make_tempcard_with_rate(RewardMatch,CountRefresh,{NNR_BOX , NR_BUFF , NR_BATTLE},{NR_BOSS,NR_ELITE,NR_MONSTER}),
					TempCardList = lists:append(HeadCardList,RewardMatch#reward_match.card_list_hid),
					TempCardList_1 = lists:append(TempCardList,CardListAdd),
					TMaxIndex = RewardMatch#reward_match.max_index,
					RewardMatch#reward_match{card_list = TempCardList_1,card_list_hid = [],max_index = TMaxIndex + CountRefresh}
			end;
		true -> 
			{TempSeelist,_} = lists:split(CountSee, HeadCardList),
			RewardMatch#reward_match{card_list = TempSeelist}
	end,

	n_make_hidcard_list(RewardMatch_1,{NNR_BOX , NR_BUFF , NR_BATTLE},{NR_BOSS,NR_ELITE,NR_MONSTER}).

%刷新翻牌
refresh_card(RewardMatch) when is_record(RewardMatch, reward_match) ->
	make_card_list(RewardMatch,
							   {?RATE_BOX,?RATE_BUFF,?RATE_BATTLE},
							   {?RATE_BOSS,?RATE_ELITE,?RATE_MONSTER},
							   true,false).

%刷新翻牌（即时buff效果）
refresh_card_buff(RewardMatch,F_R)  when is_record(RewardMatch, reward_match) ->	
	Do_Debuff = is_debuff_effect_im(F_R,?RM_BT_REF_HALFFEE,RewardMatch),
	NRewardMatch =
	if
		Do_Debuff =:= true -> 
			FEETEMP = RewardMatch#reward_match.fee_temp div 2,
			RewardMatch#reward_match{fee_temp = FEETEMP};
		true -> 
			RewardMatch
	end,

	CARD_RATE = {?RATE_BOX, ?RATE_BUFF, ?RATE_BATTLE},
	%if	
	%	F_R =:= ?RM_BT_REF_LOTTERY -> {?RATE_BOX , ?RATE_BUFF, 0};
	%	true -> {?RATE_BOX , ?RATE_BUFF , ?RATE_BATTLE}	
	%end,
	
	BATTLE_RATE = {?RATE_BOSS,?RATE_ELITE,?RATE_MONSTER},
	%if	
	%	F_R =:= ?RM_BT_REF_LOWLEVEL -> {0,0,?RATE_MONSTER};
	%	true -> {?RATE_BOSS,?RATE_ELITE,?RATE_MONSTER}	
	%end,
	
	%是否保留boss卡（默认保留，有boss去死buff则不保留）
	BossRemain = 
	if	
		F_R =:= ?RM_BT_REF_BOSSDEAD -> false;
		true -> true
	end,
	
	%是否保留倒垃圾卡（默认不保留，有倒垃圾buff则保留,只有一个格子的情况下不保留）
	TrashRemain = 
	if	
		F_R =:= ?RM_BT_REF_TRASH -> 
			if
				length(NRewardMatch#reward_match.card_list) > 1 -> true;
				true -> false
			end;
		true -> false
	end,	
	
	RewardMatch_1 = make_card_list(NRewardMatch,CARD_RATE,BATTLE_RATE,BossRemain,TrashRemain),
	
	if	
		F_R =:= ?RM_BT_REF_LOTTERY -> RewardMatch_1#reward_match{card_list = format_cardlist_lottery(RewardMatch_1)};
		F_R =:= ?RM_BT_REF_LOWLEVEL -> RewardMatch_1#reward_match{card_list = format_cardlist_lowlevel(RewardMatch_1)};
		true -> RewardMatch_1
	end.
	
format_bufflist_doctor(RewardMatch) when is_record(RewardMatch, reward_match) ->
	Predicate = fun(Buff) -> 
						Rm_battle_data = cache_csv:get_rm_buff_info(Buff#rm_buff.id),
						Rm_battle_data#rm_buff_data.isdebuff /= 1
				end,
	lists:filter(Predicate, RewardMatch#reward_match.buff_list).

format_cardlist_doctor(RewardMatch) when is_record(RewardMatch, reward_match) ->
	ok.
	
format_cardlist_lottery(RewardMatch) when is_record(RewardMatch, reward_match) -> 
	lists:map(fun(Card) -> 
					  if
							Card#rm_card.type =:= 2 ->
								Ran = dd_util:random_in_range(?RATE_BOX + ?RATE_BUFF),
						  		if
								  Ran =< ?RATE_BOX ->
									  make_box_card(Card#rm_card.index);
								  true -> 
									  make_buff_card(Card#rm_card.index,RewardMatch)
								end;
							true -> 
								Card
					  end
			  end, RewardMatch#reward_match.card_list).

format_cardlist_lowlevel(RewardMatch) when is_record(RewardMatch, reward_match) -> 
	lists:map(fun(Card) -> 
					  if
							Card#rm_card.type =:= 2 ->
								Rm_battle_data = cache_csv:get_rm_battle_info(Card#rm_card.id),
								if
									Rm_battle_data#rm_battle_data.type =:= 0 ->
										Card;
									true->
										make_battle_card(Card#rm_card.index,{0,0,?RATE_MONSTER})
								end;
							true -> 
								Card
					  end
			  end, RewardMatch#reward_match.card_list).

%%检测赏金队伍信息
check_reward_team(Account,SuperTeam) when is_record(Account,account)->
	case length(SuperTeam) of
		?MAX_TEAM_MEMBER->
			ok;
		_->
			throw({custom, "HintChangeTeamDataError"})
	end,
	%%检查球员是否存在玩家身上（暂时球员背包功能么有，先注释不做校验）
%% 	Fun = fun({_,ID})->
%% 				  case cache_util:find_player_by_id(Account#account.players#players.players_lists,ID) of
%% 					  {success,_}->
%% 						  ok;
%% 					  _->
%% 						  throw({custom,"HintSuperNotExist"})
%% 				  end
%% 			   end,
%% 	lists:foreach(Fun, SuperTeam),
	%%检测当前可不可以进行更换球员
	cache_api:check_status(Account#account.reward_match#reward_match.status,?RM_STATUS_CLOSE),
	ok.

%%替换队伍列表
update_reward_team(Account,SuperTeam)when is_record(Account,account)->
	NewTeam = Account#account.reward_match#reward_match{reward_team = SuperTeam},
	Account#account{reward_match = NewTeam}.

