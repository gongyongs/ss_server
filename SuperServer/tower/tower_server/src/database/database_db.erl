%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 七月 2014 下午3:29
%%%-------------------------------------------------------------------
-module(database_db).
-author("zqlt").
-include("../cache/cache_def.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-include("../../deps/mysql/include/mysql.hrl").
-include("../mail/mail.hrl").
-include("../csv.hrl").
%%处理数据库相关操作

%% API
-export([execute/2]).

%%查询玩家信息，如果不存在，则创建玩家信息,IsCreate表示如果没有则创建
execute(DbName, {get_account, {Uin, IsCreate}}) ->
  ?FILE_LOG_DEBUG("database_db: get_account = ~p", [Uin]),
  Sql = mysql_util:select_query("account", ["uin","player","gold_coin", "gem", "platform_info", "hero", "backpack", "mission", "achievement",
    "shop", "stage", "login_reward", "strength", "lottery", "guild", "competitive", "addition", "create_ts", "last_login_ts","reward_match","reward_match_ts"], "uin="++dd_util:to_list(Uin)),
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = [DbAccount]}} ->
      Account = database_util:decode_account_rd(DbAccount),
      {success, {Account, false}};
    {data, #mysql_result{rows = []}} ->
      %%没有找到，创建新用户
      if
        IsCreate =:= true ->
			  %%创建
              Account = database_util:init_account(Uin),
              CreateSql = database_util:get_create_account_sql(Account),
              {updated, #mysql_result{affectedrows = 1}} = mysql:fetch(DbName, CreateSql),
              {success, {Account, true}};
        true -> {fail, "uin not exist"}
      end
  end;

execute(DbName, {update_account, {Account, FieldList}}) when is_record(Account, account) andalso is_list(FieldList) ->
  ?FILE_LOG_DEBUG("dabatbase_db: update_account uin = ~p,field = ~p", [Account#account.uin, FieldList]),
  UpdateSql = database_util:update_account_sql(Account, FieldList),
  case mysql:fetch(DbName, UpdateSql) of
    {updated, #mysql_result{affectedrows = 1}} ->success;
    {updated, #mysql_result{affectedrows = 0}} ->
      ?FILE_LOG_WARNING("update affectedrows = 0", []),
      success
  end;

execute(DbName, {delete_account, Uin}) when is_integer(Uin) ->
  Sql = "delete from account where uin = " ++ dd_util:to_list(Uin),
  case mysql:fetch(DbName, Sql) of
    {updated, #mysql_result{affectedrows = 1}} -> success;
    Other ->
      ?FILE_LOG_DEBUG("delete user error, uin = ~p, reason = ~p", [Uin, Other]),
      {fail, "DeleteFail"}
  end;

execute(DbName, {get_uuid, Id}) ->
  Sql =  mysql_util:select_query("uuid", ["uuid"], "id=" ++ dd_util:to_list(Id)),
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = [Uuid]}} ->
      {success, Uuid};
    {data, #mysql_result{rows = []}} ->
      %%插入一条新数据
      InsertSql = "insert into uuid value(" ++ dd_util:to_list(Id) ++ "," ++ "1)",
      {updated, #mysql_result{affectedrows = 1}} = mysql:fetch(DbName, InsertSql),
      {success, [1]}
  end;

execute(DbName, {update_uuid, {Id, Value}}) ->
  UpdateSql = "update uuid set uuid=" ++ dd_util:to_list(Value) ++ " where id=" ++ dd_util:to_list(Id),
  case mysql:fetch(DbName, UpdateSql) of
    {updated, #mysql_result{affectedrows = 1}} -> {success, Value};
    {updated, #mysql_result{affectedrows = 0}} ->
      ?FILE_LOG_WARNING("update uuid affectedrow = 0", []),
      {success, Value}
  end;

execute(DbName, {get_all_user, _Uin}) ->
  Sql = "select uin from account",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} -> {success, []};
    {data, #mysql_result{rows = Rows}} ->
      Result = lists:map(fun([UinJson]) ->  dd_util:to_integer(UinJson) end, Rows),
      {success, Result};
    Other ->
      ?FILE_LOG_ERROR("query all user uin error, reason = ~p", [Other]),
      fail
  end;

execute(DbName, {get_uins_above_pay, Pay_count}) ->            %获取充值金额超过指定金额的玩家Uin
  ?FILE_LOG_DEBUG("go into database_db=>get_uins_above_pay  ok",[]),
  Sql = "select uin,shop from account",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} -> {success, []};
    {data, #mysql_result{rows = Rows}} ->                      %  shop::#shop{},
      All = lists:map(fun([UinJson,ShopJson]) ->  {dd_util:to_integer(UinJson),database_util:decode_shop(ShopJson)} end, Rows),
      ?FILE_LOG_DEBUG("All is ~p",[All]),
      AboveResult = lists:filter(fun({_Uin,X})->X#shop.pay_info#pay_info.total_pay_val >= Pay_count*1.0 end,All),    %筛选出充值金额大于Pay_count的数据
      Result = lists:map(fun({Uin, _Shop}) -> Uin end,AboveResult),
      ?FILE_LOG_INFO("result is ~p",[Result]),
      {success, Result};
    Other ->
      ?FILE_LOG_ERROR("query all user uin error, reason = ~p", [Other]),
      fail
  end;

execute(DbName, {get_friend_stage_info, {Uin, FriendUinList}}) ->
  SelectStageSql = "select stage from account where uin=" ++ dd_util:to_list(Uin) ++ ";",
  Stage =
    case mysql:fetch(DbName, SelectStageSql) of
      {data, #mysql_result{rows = []}} ->
        throw({custom, "get user info error"});
      {data, #mysql_result{rows = [PStageJ]}} ->
        database_util:decode_stage(PStageJ);
      _Other -> throw({custom, "get user info error"})
    end,
  case FriendUinList of
    [] -> {success, {{Uin, Stage},[]}};
    [Value] ->
      Sql = "select uin, stage from account where uin=" ++ dd_util:to_list(Value) ++ ";",
      case mysql:fetch(DbName, Sql) of
        {data, #mysql_result{rows = Rows}} ->
          Result = lists:map(
            fun([Uid, StageJson]) ->
              {dd_util:to_integer(Uid), database_util:decode_stage(StageJson)}
            end, Rows),
          {success, {{Uin, Stage}, Result}};
        OtherR ->
          ?FILE_LOG_ERROR("query friend stage info error, reason = ~p", [OtherR]),
          fail
      end;
    _ ->
      Sql = create_query_user_info_sql(FriendUinList, "stage"),
      ?FILE_LOG_INFO("query friend stage info: SQL = ~p", [Sql]),
      case mysql:fetch(DbName, Sql) of
        {data, #mysql_result{rows = Rows}} ->
          Result = lists:map(
            fun([Uid, StageJson]) ->
              {dd_util:to_integer(Uid), database_util:decode_stage(StageJson)}
            end, Rows),
          {success, {{Uin, Stage}, Result}};
        OtherRR ->
          ?FILE_LOG_ERROR("query friend stage info error, reason = ~p", [OtherRR]),
          fail
      end
  end;

execute(DbName, {get_user_stage_info, {Index, Len}}) ->
  if
    Index < 0 ->
      ?FILE_LOG_ERROR("get_user_stage_info index error", []),
      fail;
    Len =< 0 ->
      ?FILE_LOG_DEBUG("get_user_stage_info length error", []),
      fail;
    true ->
      Sql = "select uin, platform_info, stage from account limit " ++ dd_util:to_list(Index) ++ ", " ++ dd_util:to_list(Len) ++ ";",
      ?FILE_LOG_DEBUG("get_user_stage_info: sql = ~p", [Sql]),
      case mysql:fetch(DbName, Sql) of
        {data, #mysql_result{rows = Rows}} ->
          Result = lists:map(
            fun([Uin,PlatForm,StageJson]) ->
              {dd_util:to_integer(Uin), database_util:decode_platform_info(PlatForm), database_util:decode_stage(StageJson)}
            end, Rows),
          {success, Result};
        Other ->
          ?FILE_LOG_ERROR("query user stage info error, reason = ~p", [Other]),
          fail
      end
  end;

%%包括自己
execute(DbName, {get_friend_basic_info, Uin}) ->
  Sql = "select platform_info from account where uin = " ++ dd_util:to_list(Uin) ++ ";",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = [PlatformJson]}} ->
      PlatForm = database_util:decode_platform_info(PlatformJson),
      List =
        lists:map(
          fun(FriendItem) ->
            {FriendItem#friend_item.uin, FriendItem#friend_item.id, FriendItem#friend_item.dis_name}
          end, PlatForm#platform_info.player_friends),
      {success, {{Uin, PlatForm#platform_info.player_id, PlatForm#platform_info.player_dis_name}, List}};
    Other ->
      ?FILE_LOG_ERROR("query user friend info error, reason ~p", Other),
      {fail, "logic error"}
  end;

execute(DbName, {get_user_basic_info, Uin}) ->
  Sql = "select platform_info from account where uin = " ++ dd_util:to_list(Uin) ++ ";",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = [PlatformJson]}} ->
      PlatForm = database_util:decode_platform_info(PlatformJson),
      {success, {Uin, PlatForm#platform_info.player_id, PlatForm#platform_info.player_dis_name}};
    Other ->
      ?FILE_LOG_ERROR("query user friend info error, reason ~p", Other),
      {fail, "logic error"}
  end;

  %%批量获取体力赠送开关
execute(DbName, {get_energy_gift_state, UinList}) ->
  case lists:member(-1, UinList) of
    true ->
      Sql = "select uin,strength from account;",
      ?FILE_LOG_INFO("query friend strength info: SQL = ~p", [Sql]),
      case mysql:fetch(DbName, Sql) of
        {data, #mysql_result{rows = Rows}} ->
          Result = lists:map(
            fun([Uin, StrengthJson]) ->
              Strength = database_util:decode_strength(StrengthJson),
              {dd_util:to_integer(Uin), Strength#strength.close_strength_gift}
            end, Rows),
          {success, Result};
        Other ->
          ?FILE_LOG_ERROR("query friend stage info error, reason = ~p", [Other]),
          fail
      end;
    false ->
      case UinList of
        [] -> [];
        [Value] ->
          Sql = "select uin, strength from account where uin=" ++ dd_util:to_list(Value) ++ ";",
          case mysql:fetch(DbName, Sql) of
            {data, #mysql_result{rows = Rows}} ->
              Result = lists:map(
                fun([Uin, StrengthJson]) ->
                  Strength = database_util:decode_strength(StrengthJson),
                  {dd_util:to_integer(Uin), Strength#strength.close_strength_gift}
                end, Rows),
              {success, Result};
            Other ->
              ?FILE_LOG_ERROR("query friend strength info error, reason = ~p, sql = ~p", [Other, Sql]),
              fail
          end;
        _ ->
          Sql = create_query_user_info_sql(UinList, "strength"),
          ?FILE_LOG_INFO("query friend strength info: SQL = ~p", [Sql]),
          case mysql:fetch(DbName, Sql) of
            {data, #mysql_result{rows = Rows}} ->
              Result = lists:map(
                fun([Uin, StrengthJson]) ->
                  Strength = database_util:decode_strength(StrengthJson),
                  {dd_util:to_integer(Uin), Strength#strength.close_strength_gift}
                end, Rows),
              {success, Result};
            Other ->
              ?FILE_LOG_ERROR("query friend stage info error, reason = ~p", [Other]),
              fail
          end
      end
  end;
execute(DbName, {stat_get_pay_power_playdays, {Index, Len}}) ->
  if
    Index < 0 ->
      ?FILE_LOG_ERROR("get_user_pay_and_power index error", []),
      fail;
    Len =< 0 ->
      ?FILE_LOG_DEBUG("get_user_pay_and_power length error", []),
      fail;
    true ->
      Sql = "select uin, platform_info, hero, shop, login_reward, stage from account limit " ++ dd_util:to_list(Index) ++ ", " ++ dd_util:to_list(Len) ++ ";",
      ?FILE_LOG_DEBUG("get_user_pay_and_power: sql = ~p", [Sql]),
      case mysql:fetch(DbName, Sql) of
        {data, #mysql_result{rows = Rows}} ->
          Result = lists:map(
            fun([Uin, PlatFormInfo,Hero, Shop, LoginReward, Stage]) ->
              Plat = database_util:decode_platform_info(PlatFormInfo),
              {dd_util:to_integer(Uin), Plat#platform_info.player_dis_name, database_util:decode_hero(Hero), database_util:decode_shop(Shop), database_util:decode_login_reward(LoginReward), database_util:decode_stage(Stage)}
            end, Rows),
          {success, Result};
        Other ->
          ?FILE_LOG_ERROR("query user stage info error, reason = ~p", [Other]),
          fail
      end
  end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute(DbName, {get_notice_rd,_Uin}) ->
  Sql = "select notice_id,notice_title,notice_date,notice_detail,notice_pic_url,notice_sign from notice_rd;",  %notice_id,notice_title,notice_date,notice_detail,notice_pic_url
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = Notice_RdList}} ->
      ?FILE_LOG_DEBUG("notice = ~p", [Notice_RdList]),
      {success, lists:map(fun(Item) -> database_util:decode_notice_rd(Item) end, Notice_RdList)};
    Other ->
      ?FILE_LOG_ERROR("get_notice_rd error, reason ~p", Other),
      {fail, "get_notice_rd error"}
  end;

execute(DbName, {insert_notice, Notice}) ->
  Sql = database_util:create_notice_sql(Notice),
  case mysql:fetch(DbName, Sql) of
    {updated, #mysql_result{affectedrows = 1}} ->
      ?FILE_LOG_DEBUG("notice = ~p", [Notice]),
      success;
    Other ->
      ?FILE_LOG_ERROR("insert_notice error, reason ~p", [Other]),
      {fail, "insert_notice error"}
  end;

execute(DbName, {update_notice, Notice}) ->
  Sql = database_util:update_notice_sql(Notice),
  case mysql:fetch(DbName, Sql) of
    {updated, #mysql_result{affectedrows = 1}} -> success;
    {updated, #mysql_result{affectedrows = 0}} ->
      ?FILE_LOG_WARNING("update notice no update", []),
      success;
    Other ->
      ?FILE_LOG_ERROR("update error, reason ~p", Other),
      {fail, "update notcie error"}
  end;

execute(DbName, {get_notice_by_id, NoticeID}) ->
  Sql = "select notice_id, notice_title, notice_date, notice_detail, notice_pic_url,notice_sign from notice_rd where notice_id = " ++ dd_util:to_list(NoticeID) ++ ";",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} ->
      ?FILE_LOG_ERROR("query notice not exist , id = ~p", [NoticeID]),
      {fail, "NotExist"};
    {data, #mysql_result{rows = [Notice]}} ->
      ?FILE_LOG_DEBUG("notice = ~p", [Notice]),
      {success, database_util:decode_notice_rd(Notice)};
    Other ->
      ?FILE_LOG_ERROR("query_notice_by_id error, reason ~p", Other),
      {fail, "query_notice_by_id error"}
  end;
execute(DbName, {delete_notice, NoticeID}) ->
  Sql = "delete from notice_rd where notice_id = " ++ dd_util:to_list(NoticeID) ++ ";",
  case mysql:fetch(DbName, Sql) of
    {updated, #mysql_result{affectedrows = 1}} -> success;
    Other ->
      ?FILE_LOG_ERROR("delete error, notice id = ~p, reason = ~p", [NoticeID, Other]),
      {fail, "Delete Error"}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute(DbName, {get_charge_order, {Uin, Order}}) ->
  Sql = "select transaction_id,uin,transaction_ts from charge where uin =" ++ dd_util:to_list(Uin) ++ " and transaction_id='" ++ mysql_util:escape(dd_util:to_list(Order)) ++ "'",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} -> not_exist;
    {data, #mysql_result{rows = [[ID, Uid, Ts]]}} -> {success, {dd_util:to_list(ID), dd_util:to_integer(Uid), dd_util:to_integer(Ts)}};
    Other ->
      ?FILE_LOG_ERROR("query charge order error, uin = ~p, order = ~p, reason = ~p", [Uin, Order, Other]),
      {fail, "error"}
  end;
execute(DbName, {add_charge_order, {Uin, Order}}) ->
  SqlStr = lists:flatten(["insert into charge values('", mysql_util:escape(Order), "', ", mysql_util:escape(dd_util:to_list(Uin)), ", ", mysql_util:escape(dd_util:to_list(dd_util:timestamp())), ")"]),
  case mysql:fetch(DbName, SqlStr) of
    {updated, #mysql_result{affectedrows = 1}}  -> success;
    {error, {mysql_result, _, _, _, _, _, 1062, _}} -> repeat;
    _ -> fail
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%邮箱操作%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute(DbName, {alloc_mail_id, _}) ->
  generate_mail_id(DbName);
execute(DbName, {load_template_mail, _}) ->
  SelectSql = mysql_util:select_query("mail_template", ["template_id", "template_type", "template_tag", "template_title", "template_content", "template_content_param_len"], ""),
  case mysql:fetch(DbName, SelectSql) of
    {data, #mysql_result{rows = []}} ->
      {success, []};
    {data, #mysql_result{rows = TemplateRdList}} ->
      TemplateList = lists:map(fun(List) -> database_util:decode_mail_template_rd(List) end, TemplateRdList),
      {success, TemplateList};
    Other ->
      ?FILE_LOG_ERROR("load template mail error, reason = ~p", [Other]),
      {fail, "logic error"}
  end;
execute(DbName, {load_bulletin_mail, _}) ->
  SelectSql = mysql_util:select_query("bulletin_mail", ["mail_id", "mail_template_id", "mail_param", "mail_add_time", "mail_term"], ""),
  case mysql:fetch(DbName, SelectSql) of
    {data, #mysql_result{rows = []}} ->
      {success, []};
    {data, #mysql_result{rows = BulletinList}} ->
      BulletinMailList = lists:map(fun(Item) -> database_util:decode_bulletin_mail_rd(Item) end, BulletinList),
      {success, BulletinMailList};
    Other ->
      ?FILE_LOG_ERROR("load_bulletin_mail error, reason = ~p", [Other]),
      {fail, "logic error"}
  end;
execute(DbName, {get_template_mail, TemplateMailID}) ->
  SelectSql = mysql_util:select_query("mail_template", ["template_id", "template_type", "template_tag", "template_title",
    "template_content", "template_content_param_len"], "template_id='" ++ dd_util:to_list(TemplateMailID) ++ "'"),
  case mysql:fetch(DbName, SelectSql) of
    {data, #mysql_result{rows = []}} ->
      {success, []};
    {data, #mysql_result{rows = [Template]}} ->
      TemplateV = database_util:decode_mail_template_rd(Template),
      {success, TemplateV};
    Other ->
      ?FILE_LOG_ERROR("get_template_mail error, reason = ~p", [Other]),
      {fail, "logic error"}
  end;
execute(DbName, {get_template_mail_by_tag, Tag}) ->
  SelectSql = mysql_util:select_query("mail_template", ["template_id", "template_type", "template_tag", "template_title",
    "template_content", "template_content_param_len"], "template_tag='" ++ dd_util:to_list(Tag) ++ "'"),
  case mysql:fetch(DbName, SelectSql) of
    {data, #mysql_result{rows = []}} ->
      {fail, "template not exist"};
    {data, #mysql_result{rows = [Template]}} ->
      TemplateV = database_util:decode_mail_template_rd(Template),
      {success, TemplateV};
    Other ->
      ?FILE_LOG_ERROR("get_template_mail_by_tag error, reason = ~p", [Other]),
      {fail, "logic error"}
  end;
execute(DbName, {get_attach_mail, MailID}) ->
  SelectSql = mysql_util:select_query("attach_mail", ["mail_id", "mail_source", "mail_dest", "mail_template_id",
    "mail_param", "mail_attachment", "mail_add_time", "mail_type", "mail_term"], "mail_id=" ++ dd_util:to_list(MailID)),
  case mysql:fetch(DbName, SelectSql) of
    {data, #mysql_result{rows = []}} ->
      not_exist;
    {data, #mysql_result{rows = [AttachMail]}} ->
      AttachMainV = database_util:decode_attach_mail_rd(AttachMail),
      {success, AttachMainV};
    Other ->
      ?FILE_LOG_ERROR("get_attach_mail error, reason = ~p", [Other]),
      {fail, "logic error"}
  end;
execute(DbName, {get_bulletin_mail, MailID}) ->
  SelectSql = mysql_util:select_query("bulletin_mail", ["mail_id", "mail_template_id", "mail_param", "mail_add_time", "mail_term"], "mail_id=" ++ dd_util:to_list(MailID)),
  case mysql:fetch(DbName, SelectSql) of
    {data, #mysql_result{rows = []}} ->
      not_exist;
    {data, #mysql_result{rows = [BulletinMail]}} ->
      BulletinMailV = database_util:decode_bulletin_mail_rd(BulletinMail),
      {success, BulletinMailV};
    Other ->
      ?FILE_LOG_ERROR("get_bulletin_mail error, reason = ~p", [Other]),
      {fail, "logic error"}
  end;

execute(DbName, {get_attach_mail_by_uin, Uin}) ->
%  SelectSql = mysql_util:select_query("attach_mail", ["mail_id", "mail_source", "mail_dest", "mail_template_id",
%    "mail_param", "mail_attachment", "mail_add_time", "mail_type", "mail_term"], "mail_dest=" ++ dd_util:to_list(Uin)),
  SelectSql = "select mail_id,mail_source,mail_dest,mail_template_id,mail_param,mail_attachment,mail_add_time,mail_type,mail_term from attach_mail where mail_dest="
    ++dd_util:to_list(Uin)++" order by mail_add_time;",
  case mysql:fetch(DbName, SelectSql) of
    {data, #mysql_result{rows = []}} ->
      {success, []};
    {data, #mysql_result{rows = AttachList}} ->
      AttachMailList = lists:map(fun(Item) -> database_util:decode_attach_mail_rd(Item) end, AttachList),
      {success, AttachMailList};
    Other ->
      ?FILE_LOG_ERROR("get attach mail by uin error, reason = ~p", [Other]),
      {fail, "logic error"}
  end;

execute(DbName, {delete_mass_attach_mail, MailIDList}) ->
  case MailIDList of
    [] -> success;
    [ID] ->
      Sql = "delete from attach_mail where mail_id=" ++ dd_util:to_list(ID) ++ ";",
      case mysql:fetch(DbName, Sql) of
        {updated, #mysql_result{affectedrows = 1}} -> success;
        Other ->
          ?FILE_LOG_ERROR("delete attach_mail error, reason = ~p, sql = ~p", [Other, Sql]),
          fail
      end;
    _ ->
      Sql = delete_sql_with_list(MailIDList, "mail_id", "attach_mail"),
      ?FILE_LOG_INFO("delete mass attach mail info: SQL = ~p", [Sql]),
      Len = length(MailIDList),
      case mysql:fetch(DbName, Sql, 10000) of
        {updated, #mysql_result{affectedrows = Len}} -> success;
        Other ->
          ?FILE_LOG_ERROR("delete mass attach mail error, reason = ~p", [Other]),
          fail
      end
  end;

%%/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
execute(DbName, {filter_friend_mail_by_uin, Uin}) ->
  SelectSql = "select mail_id from attach_mail where mail_dest=" ++dd_util:to_list(Uin)++" and mail_type = 1 order by mail_add_time desc;",      %%降序
  case mysql:fetch(DbName, SelectSql) of
    {data, #mysql_result{rows = []}} -> success;
    {data, #mysql_result{rows = AttachList}} ->
      AttachMailList = lists:map(fun([ID]) -> dd_util:to_integer(ID) end, AttachList),
      case filter_mail(AttachMailList) of
        [] -> success;
        DeleteList ->
          Sql = get_delete_sql(DeleteList, "attach_mail"),
          case mysql:fetch(DbName, Sql) of
            {updated, #mysql_result{affectedrows = 0}} -> fail;
            {updated,#mysql_result{affectedrows = Row}} ->
              ?FILE_LOG_DEBUG("delete rows ~p", [Row]),
              success;
            Error ->
              ?FILE_LOG_DEBUG("delete friend amount mail error, uin = ~p, delete list = ~p, reason = ~p", [Uin, DeleteList, Error]),
              fail
          end
      end;
    Other ->
      ?FILE_LOG_ERROR("get friend mail by uin error, reason = ~p", [Other]),
      fail
  end;

execute(DbName, {filter_system_mail_by_uin, Uin}) ->
  SelectSql = "select mail_id from attach_mail where mail_dest=" ++dd_util:to_list(Uin)++" and mail_type = 2 order by mail_add_time desc;",
  case mysql:fetch(DbName, SelectSql) of
    {data, #mysql_result{rows = []}} -> success;
    {data, #mysql_result{rows = AttachList}} ->
      AttachMailList = lists:map(fun([ID]) -> dd_util:to_integer(ID) end, AttachList),
      case filter_mail(AttachMailList) of
        [] -> success;
        DeleteList ->
          Sql = get_delete_sql(DeleteList, "attach_mail"),
          case mysql:fetch(DbName, Sql) of
            {updated, #mysql_result{affectedrows = 0}} -> fail;
            {updated,#mysql_result{affectedrows = Row}} ->
              ?FILE_LOG_DEBUG("delete rows ~p", [Row]),
              success;
            Error ->
              ?FILE_LOG_DEBUG("delete system amount mail error, uin = ~p, delete list = ~p, reason = ~p", [Uin, DeleteList, Error]),
              fail
          end
      end;
    Other ->
      ?FILE_LOG_ERROR("get attach mail by uin error, reason = ~p", [Other]),
      fail
  end;

execute(DbName, {filter_mail_overtime, _U}) ->
  DelAttachSql = "delete from attach_mail where mail_add_time < (unix_timestamp() - mail_term*24*60*60);",
  DelBulletinSql = "delete from bulletin_mail where mail_add_time < (unix_timestamp() - mail_term*24*60*60);",
  case mysql:fetch(DbName, DelAttachSql) of
    {updated, #mysql_result{affectedrows = AttachRow}} ->
      ?FILE_LOG_DEBUG("delete overtime attach mail ~p", [AttachRow]),
      case mysql:fetch(DbName, DelBulletinSql) of
        {updated, #mysql_result{affectedrows = BulletinRow}} ->
          ?FILE_LOG_DEBUG("delete overtime bulletin mail ~p", [BulletinRow]),
          success;
        Error ->
          ?FILE_LOG_ERROR("filter_mail_overtime=> del bulletin mail reason = ~p", [Error]),
          fail
      end;
    Other ->
      ?FILE_LOG_ERROR("filter_mail_overtime=>del attach mail error, reason = ~p", [Other]),
      fail
  end;
%%%/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
execute(DbName, {delete_bulletin_mail, MailID}) ->
  DelSql = "delete from bulletin_mail where mail_id=" ++ dd_util:to_list(MailID),
  case mysql:fetch(DbName, DelSql) of
    {updated, #mysql_result{affectedrows = 1}} -> success;
    Other ->
      ?FILE_LOG_DEBUG("~p", [Other]),
      fail
  end;
execute(DbName, {delete_attach_mail, MailID}) ->
  DelSql = "delete from attach_mail where mail_id=" ++ dd_util:to_list(MailID),
  ?FILE_LOG_DEBUG("delete attach mail : sql = ~p", [DelSql]),
  case mysql:fetch(DbName, DelSql) of
    {updated, #mysql_result{affectedrows = 1}} -> success;
    Other ->
      ?FILE_LOG_DEBUG("~p", [Other]),
      fail
  end;
execute(DbName, {delete_template_mail, TemplateID}) ->
  DelSql = "delete from mail_template where template_id='" ++ dd_util:to_list(TemplateID) ++ "'",
  DelAttachSql = "delete from attach_mail where mail_template_id='" ++ dd_util:to_list(TemplateID) ++ "'",
  DelBulletinSql = "delete from bulletin_mail where mail_template_id='" ++ dd_util:to_list(TemplateID) ++ "'",
  case mysql:fetch(DbName, DelSql) of
    {updated, #mysql_result{affectedrows = 1}} ->
      case mysql:fetch(DbName, DelAttachSql) of
        {updated, _} ->
          case mysql:fetch(DbName, DelBulletinSql) of
            {updated, _} ->
              success;
            OtherEE ->
              ?FILE_LOG_DEBUG("delete_template_mail  => DelBulletinSql: ~p", [OtherEE]),
              fail
          end;
        OtherE ->
          ?FILE_LOG_DEBUG("delete_template_mail => DelAttachSql: ~p", [OtherE]),
          fail
      end;
    Other ->
      ?FILE_LOG_DEBUG("delete_template_mail: ~p", [Other]),
      fail
  end;

execute(DbName, {insert_template_mail, TemplateMail}) when is_record(TemplateMail, template_mail) ->
  Sql = database_util:get_create_mail_template_sql(TemplateMail),
  case mysql:fetch(DbName, Sql) of
    {updated, #mysql_result{affectedrows = 1}} ->
      success;
    Other ->
      ?FILE_LOG_ERROR("insert template mail error ,reason => ~p", [Other]),
      fail
  end;
execute(DbName, {insert_bulletin_mail, BulletinMail}) when is_record(BulletinMail, bulletin_mail) ->
  {success, MailID} = generate_mail_id(DbName),
  NBulletinMail = BulletinMail#bulletin_mail{mail_id = MailID},
  Sql = database_util:get_create_bulletin_mail_sql(NBulletinMail),
  case mysql:fetch(DbName, Sql) of
    {updated, #mysql_result{affectedrows = 1}} ->
      {success, MailID};
    Other ->
      ?FILE_LOG_ERROR("insert_bulletin_mail error ,reason => ~p", [Other]),
      fail
  end;
execute(DbName, {insert_attach_mail, AttachMail}) when is_record(AttachMail, attach_mail) ->
  case check_user(DbName, AttachMail#attach_mail.mail_dest) of
    {fail, Reason} ->
      ?FILE_LOG_ERROR("insert_attach_mail :check user error, reason = ~p, uin = ~p", [Reason, AttachMail#attach_mail.mail_dest]),
      fail;
    {success, _} ->
      {success, MailID} = generate_mail_id(DbName),
      NAttachMail = AttachMail#attach_mail{mail_id = MailID},
      Sql = database_util:get_create_attach_mail_sql(NAttachMail),
      case mysql:fetch(DbName, Sql) of
        {updated, #mysql_result{affectedrows = 1}} ->
          {success, MailID};
        Other ->
          ?FILE_LOG_ERROR("insert_attach_mail error ,reason => ~p", [Other]),
          fail
      end
  end;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute(DbName, {add_shop_item, GoodItem}) when is_record(GoodItem, res_goods)->
  Sql = database_util:get_create_good_sql(GoodItem),
  case mysql:fetch(DbName, Sql) of
    {updated, #mysql_result{affectedrows = 1}} ->
      success;
    Other ->
      ?FILE_LOG_ERROR("insert shop item error ,reason => ~p", [Other]),
      fail
  end;

execute(DbName, {delete_shop_item,  Shop_id}) when is_list(Shop_id) ->
  DelSql = "delete from shop_config where id='" ++ Shop_id ++ "';",
  case mysql:fetch(DbName, DelSql) of
    {updated, #mysql_result{affectedrows = 0}} ->
      ?FILE_LOG_DEBUG("shop item not exits, id = ~p",[Shop_id]),
      success;
    {updated, #mysql_result{affectedrows = 1}} -> success;
    Other ->
      ?FILE_LOG_DEBUG("~p", [Other]),
      {fail,"delete error"}
  end;

execute(DbName, {query_all_shop_item, _Shop_id}) ->
  Sql = "select * from shop_config; ",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} -> {success, []};
    {data, #mysql_result{rows = RowList1}} ->
      RowList = lists:map(fun(Item) -> database_util:decode_shop_config_item(Item) end, RowList1),
      {success,RowList};
    Other ->
      ?FILE_LOG_ERROR("query shop item error ,reason => ~p", [Other]),
      fail
  end;

execute(DbName, {query_all_lottery_item, _Shop_id}) ->            %%查找所有抽奖项
  Sql = "select * from lottery_discount; ",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} -> {success, []};
    {data, #mysql_result{rows = RowList1}} ->
      RowList = lists:map(fun(Item) -> database_util:decode_lottery_config_item(Item) end, RowList1),
      {success,RowList};
    Other ->
      ?FILE_LOG_ERROR("query lottery item error ,reason => ~p", [Other]),
      fail
  end;

execute(DbName, {query_lottery_item_by_id, Lottery_id}) ->            %%查找所有抽奖项
  Sql = "select * from lottery_discount where id = '" ++ dd_util:to_list(Lottery_id) ++ "';",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} -> {success, []};
    {data, #mysql_result{rows = RowList1}} ->
      RowList = lists:map(fun(Item) -> database_util:decode_lottery_config_item(Item) end, RowList1),
      {success,RowList};
    Other ->
      ?FILE_LOG_ERROR("query lottery item error ,reason => ~p", [Other]),
      fail
  end;

execute(DbName, {query_shop_item_by_id, Shop_id}) when is_list(Shop_id) ->
  Sql = "select * from shop_config where id = '" ++ dd_util:to_list(Shop_id) ++ "';",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} ->
      not_exist;
    {data, #mysql_result{rows = Rows}} ->
      {success, database_util:decode_shop_config_item(Rows)};
    Other ->
      ?FILE_LOG_ERROR("insert shop item error ,reason => ~p", [Other]),
      fail
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute(DbName, {get_model_state, Model_id})when is_list(Model_id) ->     %获取各模块功能状态，number为1表示开启，number为0表示关闭
  ?FILE_LOG_DEBUG("go into database_db => get_model_state,Model_id = ~p",[Model_id]),
  Sql = "select state_num from model_state where model_id = '" ++ Model_id ++ "';",
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} ->
      not_exist;
    {data, #mysql_result{rows = Rows}} ->
      [[Number]] = Rows,
      {success, Number};
    Other ->
      ?FILE_LOG_ERROR("get_model_state error ,reason => ~p", [Other]),
      {fail,Other}
  end;

execute(DbName, {change_model_state, Action}) ->   %改变各模块功能状态，num为1表示开启，num为0表示关闭
  ?FILE_LOG_DEBUG("go into database_db => change_model_state,Action = ~p",[Action]),
  {State_num,Model_id} = action_to_statenum(Action),
  Sql =  "update model_state set state_num = "++State_num++"where model_id = '"++Model_id++"';",
  CheckSql = "select state_num from model_state where model_id = '"++Model_id++"';",
  case mysql:fetch(DbName, CheckSql) of
    {data, #mysql_result{rows = []}} -> {fail, ["no such model"]};
    {data, #mysql_result{rows = Rows}} ->
      ?FILE_LOG_DEBUG("Rows is ~p",[Rows]),
      [[Number]]=Rows,
      ?FILE_LOG_DEBUG("Number is ~p,Num is ~p",[Number,State_num]),
      if
        Number =:= State_num ->
           ?FILE_LOG_DEBUG(" Number =:= Num",[]),
          {fail,"function has been stoped/started."};
        true ->
          case mysql:fetch(DbName, Sql) of
          {updated, #mysql_result{affectedrows=1}} ->
          success;
          _ ->
          {fail, "sys error"}
          end
      end;
    Other ->
      ?FILE_LOG_ERROR("change function state error, reason = ~p", [Other]),
      fail
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action_to_statenum(Action)->
  case Action of
    start_endless -> {1,endless};
    start_reward -> {1,reward};
    start_treasure -> {1,treasure};
    start_upg -> {1,upg};
    start_eva -> {1,eva};
    start_mail -> {1,mail};
    start_shop -> {1,shop};
    stop_endless -> {0,endless};
    stop_reward -> {0,reward};
    stop_treasure -> {0,treasure};
    stop_upg -> {0,upg};
    stop_eva -> {0,eva};
    stop_mail -> {0,mail};
    stop_shop -> {0,shop};
    _ -> throw({custom,"action not exist!"})
  end.

create_query_user_info_sql(UinList, Info) when is_list(Info) andalso length(Info) > 0 ->
  create_query_user_info_sql_1(UinList, "select uin, " ++ dd_util:to_list(Info) ++ " from account where uin in (").
create_query_user_info_sql_1([], Sql) -> Sql;
create_query_user_info_sql_1([ID], Sql) -> Sql ++ dd_util:to_list(ID) ++ ");";
create_query_user_info_sql_1([H | T], Sql) ->
  create_query_user_info_sql_1(T, Sql ++ dd_util:to_list(H) ++ ",").


generate_mail_id(DbName) ->
  Sql1 = "select freeid from mail_id;",
  case mysql:fetch(DbName, Sql1) of
    {data, #mysql_result{rows = [[MailID]]}} ->
      Sql = "update mail_id set `freeid` = `freeid`+1",
      case mysql:fetch(DbName, Sql) of
        {updated, #mysql_result{affectedrows=1}} -> {success, dd_util:to_integer(MailID)};
        _ ->
          {fail, "sys error"}
      end;
    {data,#mysql_result{rows=[]}} -> {fail, "sys error"}
  end.

check_user(DbName, Uin) ->
  Sql = "select uin from account where uin = " ++ dd_util:to_list(Uin),
  case mysql:fetch(DbName, Sql) of
    {data, #mysql_result{rows = []}} -> {fail, "account not exist"};
    {data, #mysql_result{rows = [[_]]}} -> {success, Uin};
    Other ->
      ?FILE_LOG_ERROR("check_user ~p error, reason = ~p", [Uin, Other]),
      {fail, "sys error"}
  end.

%%筛选邮件不大于30份，超过的从数据库中删除
filter_mail(MailList)->
  Length = length(MailList),
  if
    Length =< 30 -> [];
    true -> lists:sublist(MailList, 31, Length - 30)
  end.

get_delete_sql(List, TableName) ->
  case List of
    [ID] -> "delete from " ++ dd_util:to_list(TableName) ++ " where mail_id = " ++ dd_util:to_list(ID) ++ ";";
    _ -> delete_sql_with_list(List, "mail_id", TableName)
  end.

%% select_sql_with_list(KeyList, KeyName, QueryInfo, TableName) when is_list(QueryInfo) andalso length(QueryInfo) > 0 andalso is_list(KeyName) ->
%%   select_sql_with_list_1(KeyList, "select " ++ dd_util:to_list(QueryInfo) ++ " from " ++ TableName ++ " where " ++ KeyName ++ " in (").
%% select_sql_with_list_1([], Sql) -> Sql;
%% select_sql_with_list_1([ID], Sql) -> Sql ++ dd_util:to_list(ID) ++ ");";
%% select_sql_with_list_1([H | T], Sql) ->
%% select_sql_with_list_1(T, Sql ++ dd_util:to_list(H) ++ ",").

delete_sql_with_list(KeyList, KeyName, TableName) when is_list(KeyName) andalso is_list(TableName) ->
  delete_sql_with_list_1(KeyList, "delete from " ++ TableName ++ " where " ++ KeyName ++ " in (").
delete_sql_with_list_1([], Sql) -> Sql;
delete_sql_with_list_1([ID], Sql) -> Sql ++ dd_util:to_list(ID) ++ ");";
delete_sql_with_list_1([H | T], Sql) ->
delete_sql_with_list_1(T, Sql ++ dd_util:to_list(H) ++ ",").



