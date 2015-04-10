%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 九月 2014 下午7:09
%%%-------------------------------------------------------------------
-module(mail_work_impl).
-author("zqlt").
-include("mail.hrl").
-include("../csv.hrl").
-include("../../deps/file_log/include/file_log.hrl").

%% API
-export([execute/2]).
-export([send_feedback_mail/2]).

execute(get_mail, Uin) ->
   case mail_db:get_attach_mail(Uin) of
     {success, AttachMailList} ->
       case mail_db:get_bulletin_mail() of
         {success, BulletinList} ->
           {
             success,
             {
               mail_util:assemble_attach_mail(AttachMailList),
               mail_util:assemble_bulletin_mail(BulletinList)
             }
           };
         {fail, NReason} ->
           ?FILE_LOG_DEBUG("get bulletin mail error, reaon = ~p", [NReason]),
           {fail, "HintSystemError"}
       end;
     {fail, Reason} ->
       ?FILE_LOG_DEBUG("get attach mail error, reason = ~p", [Reason]),
       {fail, "HintSystemError"}
   end;

execute(get_assemble_bulletin_mail, _) ->
  case mail_db:get_bulletin_mail() of
    {success, BulletinList} -> {success,  mail_util:assemble_bulletin_mail(BulletinList)};
    {fail, NReason} ->
      ?FILE_LOG_DEBUG("get bulletin mail error, reaon = ~p", [NReason]),
      {fail, "HintSystemError"}
  end;


execute(get_attach_mail_by_uin, Uin) ->
  case mail_db:get_attach_mail(Uin) of
    {success, AttachMailList} -> {success, AttachMailList};
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("get attach mail error, reason = ~p", [Reason]),
      {fail, "HintSystemError"}
  end;

 %%删除大量邮件
execute(delete_mass_attach_mail, MailIDList) when is_list(MailIDList) andalso length(MailIDList) > 0 ->
  case mail_db:del_mass_attach_mail(MailIDList) of
    success -> success;
    fail -> fail
  end;

execute(present_friend_strength, {SourceUin, DestUin}) ->
  Tag = "power_gift",
  case mail_template:get_template_by_tag(Tag) of
    {success, Template} ->
      case Template#template_mail.template_type of
        1 -> %%读取配置表
          case mail_template:load_mail_template_config_by_id(Template#template_mail.template_id) of
            {success, TemplateConfig} ->
              Attach = #mail_attachment{property_id = TemplateConfig#res_template_mail_config.tool_id, type = TemplateConfig#res_template_mail_config.attach_type, count = TemplateConfig#res_template_mail_config.attach_num},
              ParamList =
                if
                  Template#template_mail.template_content_parm_len =:= 1 -> [TemplateConfig#res_template_mail_config.attach_num];   %%设计缺陷，暂时这样处理
                  true ->  lists:map(fun(_) -> " " end, lists:seq(1, Template#template_mail.template_content_parm_len))   %%如果自动填充的邮件参数 超过1个，则全部填写空格
                end,
              AttachMail = #attach_mail{mail_id = 0, mail_source = SourceUin, mail_dest = DestUin, mail_template_id = Template#template_mail.template_id,
              mail_attachment = Attach, mail_param_list = ParamList, mail_add_ts = dd_util:timestamp(), mail_type = 1, mail_term = TemplateConfig#res_template_mail_config.term},
              mail_db:insert_attach_mail(AttachMail);
            {fail, Reason} ->
              ?FILE_LOG_DEBUG("present friend strength => load mail template config by id error, reason = ~p", [Reason]),
              {fail, "HintSystemError"}
          end;
        2 -> %%自动填写(由管理员填写)
          Attach = #mail_attachment{property_id = [], type = 3, count = 1},
          ParamList = lists:map(fun(_) -> " " end, lists:seq(1, Template#template_mail.template_content_parm_len)),
          AttachMail = #attach_mail{mail_id = 0, mail_source = SourceUin, mail_dest = DestUin, mail_template_id = Template#template_mail.template_id,
          mail_attachment = Attach, mail_param_list = ParamList, mail_add_ts = dd_util:timestamp(), mail_type = 1, mail_term = 7},
          mail_db:insert_attach_mail(AttachMail)
      end;
    fail ->
      ?FILE_LOG_DEBUG("get_template_by_tag: error, tag = ~p", [Tag]),
      {fail, "HintSystemError"}
  end;
%%需要校验did是否存在，此校验过程放在数据库部分处理
execute(add_attach_mail,  {Sid, Did, MailTemplateID, MailParamList, {Type, PropertyId, Count}, Term})when is_integer(Sid) andalso is_integer(Did) andalso is_list(MailTemplateID) andalso is_integer(Term) andalso Term > 0 andalso is_list(MailParamList) ->
  ?FILE_LOG_DEBUG("mail_work_impl=>execute=>add_attach_mail MailTemplateID is ~p", [MailTemplateID]),
  case mail_template:verify_mail_template(MailTemplateID, MailParamList) of
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("verfify mail template error, reason = ~p, [~p, ~p]", [Reason, MailTemplateID, MailParamList]),
      {fail, "HintSystemError"};
    {success, _} ->
      ?FILE_LOG_DEBUG("success=>mail_work_impl=>execute=>add_attach_mail", []),
      AttachMail =
        case Sid of
          -1 -> #attach_mail{mail_id = 0, mail_source = Sid, mail_dest = Did, mail_template_id = MailTemplateID, mail_attachment = #mail_attachment{type = Type, property_id = PropertyId, count = Count}, mail_param_list = MailParamList, mail_term = Term, mail_type = 2, mail_add_ts = dd_util:timestamp()};
          _ ->  #attach_mail{mail_id = 0, mail_source = Sid, mail_dest = Did, mail_template_id = MailTemplateID, mail_attachment = #mail_attachment{type = Type, property_id = PropertyId, count = Count}, mail_param_list = MailParamList, mail_term = Term, mail_type = 1, mail_add_ts = dd_util:timestamp()}
        end,
      mail_db:insert_attach_mail(AttachMail)
  end;
execute(add_bulletin_mail,  {MailTemplateID, MailParamList, Term}) when is_list(MailTemplateID) andalso is_list(MailParamList) andalso is_integer(Term) andalso Term > 0 ->
  case mail_template:verify_mail_template(MailTemplateID, MailParamList) of
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("verfify mail template error, reason = ~p, [~p, ~p]", [Reason, MailTemplateID, MailParamList]),
      {fail, "HintSystemError"};
    {success, _} ->
      BulletinMail = #bulletin_mail{mail_id = 0, mail_template_id = MailTemplateID, mail_param_list = MailParamList, mail_add_ts = dd_util:timestamp(), mail_term = Term},
      ?FILE_LOG_DEBUG("mail_work_impl=>add_bullet_mail=>BulletinMail=~p", [BulletinMail]),
      mail_db:insert_bulletin_mail(BulletinMail)
  end;
execute(get_attach_mail, MailID) when is_integer(MailID) ->
  mail_db:get_attach_mail_by_id(MailID);
execute(get_bulletin_mail, MailID) when is_integer(MailID) ->
  mail_db:get_bulletin_mail_by_id(MailID);
execute(del_attach_mail, MailID) when is_integer(MailID) ->
  mail_db:del_attach_mail(MailID);
execute(del_bulletin_mail, MailID) when is_integer(MailID) ->
  mail_db:del_bulletin_mail(MailID);

execute(server_rank_reward, {Uin, RankPercent, Rank}) ->
  Tag =
    case RankPercent of
      10 -> "worldrank_first";
      20 -> "worldrank_second";
      30 -> "worldrank_third";
      _ -> throw({custom, "HintSystemError"})
    end,
  case mail_template:get_template_by_tag(Tag) of
    {success, Template} ->
      case Template#template_mail.template_type of
        1 -> %%读取配置表
          case mail_template:load_mail_template_config_by_id(Template#template_mail.template_id) of
            {success, TemplateConfig} ->
              Attach = #mail_attachment{property_id = TemplateConfig#res_template_mail_config.tool_id, type = TemplateConfig#res_template_mail_config.attach_type, count = TemplateConfig#res_template_mail_config.attach_num},
              ParamList =
                if
                  Template#template_mail.template_content_parm_len =:= 1 -> [TemplateConfig#res_template_mail_config.attach_num];
                  true -> lists:map(fun(_) -> " " end, lists:seq(1, Template#template_mail.template_content_parm_len))
                end,
              AttachMail = #attach_mail{mail_id = 0, mail_source = -1, mail_dest = Uin, mail_template_id = Template#template_mail.template_id,
              mail_attachment = Attach, mail_param_list = ParamList, mail_add_ts = dd_util:timestamp(), mail_type = 2, mail_term = TemplateConfig#res_template_mail_config.term},
              mail_db:insert_attach_mail(AttachMail);
            {fail, Reason} ->
              ?FILE_LOG_DEBUG("server_rank_reward => load mail template config by id error, reason = ~p", [Reason]),
              {fail, "HintSystemError"}
          end;
        2 -> %%自动填写
          Attach = #mail_attachment{property_id = [], type = 3, count = 1},
          ParamList = lists:map(fun(_) -> " " end, lists:seq(1, Template#template_mail.template_content_parm_len)),
          AttachMail = #attach_mail{mail_id = 0, mail_source = -1, mail_dest = Uin, mail_template_id = Template#template_mail.template_id,
          mail_attachment = Attach, mail_param_list = ParamList, mail_add_ts = dd_util:timestamp(), mail_type = 2, mail_term = 7},
          mail_db:insert_attach_mail(AttachMail)
      end;
    fail ->
      ?FILE_LOG_DEBUG("server_rank_reward error=> uin=~p, rankpercent = ~p, rank =~p", [Uin, RankPercent, Rank]),
      {fail, "HintSystemError"}
  end;
execute(friend_rank_reward, {Uin, Rank}) ->
  Tag =
    case Rank of
      1 -> "fiendsrank_first";
      2 -> "fiendsrank_second";
      3 -> "fiendsrank_third";
      _ ->
        ?FILE_LOG_DEBUG("rank data error ,[~p, ~p]", [Uin, Rank]),
        throw({custom, "HintSystemError"})
    end,
  case mail_template:get_template_by_tag(Tag) of
    {success, Template} ->
      case Template#template_mail.template_type of
        1 -> %%读取配置表
          case mail_template:load_mail_template_config_by_id(Template#template_mail.template_id) of
            {success, TemplateConfig} ->
              Attach = #mail_attachment{property_id = TemplateConfig#res_template_mail_config.tool_id, type = TemplateConfig#res_template_mail_config.attach_type, count = TemplateConfig#res_template_mail_config.attach_num},
              ParamList =
                if
                  Template#template_mail.template_content_parm_len =:= 1 -> [TemplateConfig#res_template_mail_config.attach_num];
                  true -> lists:map(fun(_) -> " " end, lists:seq(1, Template#template_mail.template_content_parm_len))
                end,
              AttachMail = #attach_mail{mail_id = 0, mail_source = -1, mail_dest = Uin, mail_template_id = Template#template_mail.template_id,
              mail_attachment = Attach, mail_param_list = ParamList, mail_add_ts = dd_util:timestamp(), mail_type = 2, mail_term = TemplateConfig#res_template_mail_config.term},
              mail_db:insert_attach_mail(AttachMail);
            {fail, Reason} ->
              ?FILE_LOG_DEBUG("friend_rank_reward => load mail template config by id error, reason = ~p", [Reason]),
              {fail, "HintSystemError"}
          end;
        2 -> %%自动填写
          Attach = #mail_attachment{property_id = [], type = 3, count = 1},
          ParamList = lists:map(fun(_) -> " " end, lists:seq(1, Template#template_mail.template_content_parm_len)),
          AttachMail = #attach_mail{mail_id = 0, mail_source = -1, mail_dest = Uin, mail_template_id = Template#template_mail.template_id,
          mail_attachment = Attach, mail_param_list = ParamList, mail_add_ts = dd_util:timestamp(), mail_type = 2, mail_term = 7},
          mail_db:insert_attach_mail(AttachMail)
      end;
    fail ->
      ?FILE_LOG_DEBUG("friend_rank_reward error=> uin=~p, rank =~p", [Uin, Rank]),
      {fail, "HintSystemError"}
  end;

%%校验source_id和dest_id
execute(send_mass_attach_mail,  {Sid, DidList, MailTemplateID, MailParamList, {Type, PropertyId, Count}, Term})when is_integer(Sid) andalso is_list(DidList) andalso is_list(MailTemplateID) andalso is_integer(Term) andalso Term > 0 andalso is_list(MailParamList) ->
  ?FILE_LOG_DEBUG("mail_work_impl=>execute=>send_mass_attach_mail MailTemplateID is ~p", [MailTemplateID]),
  case mail_template:verify_mail_template(MailTemplateID, MailParamList) of
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("verfify mail template error, reason = ~p, [~p, ~p]", [Reason, MailTemplateID, MailParamList]),
      {fail, "HintSystemError"};
    {success, _} ->
      ?FILE_LOG_DEBUG("success=>mail_work_impl=>execute=>send_mass_attach_mail", []),
      DestUinList =
        case lists:member(-1, DidList) of
          true ->
            case mail_db:get_all_user() of
              {success, L} -> L;
              _ ->
                ?FILE_LOG_DEBUG("send_mass_attach_mail => get_all_user error", []),
                throw({custom, "HintSystemError"})
            end;
          false ->
            %%校验玩家
            lists:map(
              fun(ID) ->
                case mail_util:get_source_info(ID) of
                  {success, _} -> ID;
                  _ ->
                    ?FILE_LOG_DEBUG("get source info error, sid = ~p", [ID]),
                    throw({custom, "HintSystemError"})
                end
              end, DidList)
        end,
      %%校验source_id
      case mail_util:get_source_info(Sid) of
        {success, _ } -> ok;
        _ ->
          ?FILE_LOG_DEBUG("send_mass_attach_mail error, get source info error, source_id = ~p", [Sid]),
          throw({custom, "HintSystemError"})
      end,
      {SuccessList, FailList} =
        lists:foldr(
          fun(ID, {TmpSuccessList, TmpFailList}) ->
            AttachMail =
              case Sid of
                -1 -> #attach_mail{mail_id = 0, mail_source = Sid, mail_dest = ID, mail_template_id = MailTemplateID, mail_attachment = #mail_attachment{type = Type, property_id = PropertyId, count = Count}, mail_param_list = MailParamList, mail_term = Term, mail_type = 2, mail_add_ts = dd_util:timestamp()};
                _ ->  #attach_mail{mail_id = 0, mail_source = Sid, mail_dest = ID, mail_template_id = MailTemplateID, mail_attachment = #mail_attachment{type = Type, property_id = PropertyId, count = Count}, mail_param_list = MailParamList, mail_term = Term, mail_type = 1, mail_add_ts = dd_util:timestamp()}
              end,
            case mail_db:insert_attach_mail(AttachMail) of
              {success, _ } -> {[ID | TmpSuccessList], TmpFailList};
              FailReason ->
                ?FILE_LOG_DEBUG("insert_attach_mail fail, source_id = ~p, dest_id = ~p, reason = ~p", [Sid, ID, FailReason]),
                {TmpSuccessList, [ID | TmpFailList]}
            end
          end, {[], []}, DestUinList),
      ?FILE_LOG_DEBUG("send_mass_attach_mail: source_id = ~p, template_id =~p, attach_type = ~p, attach_id = ~p, attach_count = ~p, success_list = ~p, fail_list = ~p", [Sid, MailTemplateID,Type, PropertyId, Count, SuccessList, FailList]),
      {success, {SuccessList, FailList}}
  end.

send_feedback_mail(FeedBackData, Uin) ->
  ?FILE_LOG_DEBUG("MAIL_WORK_IMPL send_feedback_mail ",[]),
  mail_feedback_smtp:send(
    #email{
      server_ip   = ?SMTP_SERVER_IP,
      account     = ?SEND_FEEDBACK_ACCOUNT,
      password    = ?SEND_FEEDBACK_PASSWORD,
      subject     = "feedback mail & GameID is"++dd_util:to_list(Uin),
      text        = dd_util:to_binary(FeedBackData),
      to_emails   = [?SEND_TO_ACCOUNT]
    }).

