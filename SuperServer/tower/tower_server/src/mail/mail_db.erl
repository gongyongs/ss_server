%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 九月 2014 下午1:45
%%%-------------------------------------------------------------------
-module(mail_db).
-author("zqlt").
-include("../../deps/file_log/include/file_log.hrl").
-include("mail.hrl").
%% API
-export([
  get_bulletin_mail/0,
  get_attach_mail/1,
  get_attach_mail_by_id/1,
  get_bulletin_mail_by_id/1,
  insert_bulletin_mail/1,
  insert_attach_mail/1,
  del_bulletin_mail/1,
  del_attach_mail/1,
  get_all_user/0,
  filter_mail_from_db/2,
  filter_mail_overtime/0,
  del_mass_attach_mail/1
]).

get_attach_mail_by_id(MailID) when is_integer(MailID) ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [get_attach_mail, MailID]) of
    {success, AttachMail} -> {success, AttachMail};
    not_exist ->
      ?FILE_LOG_ERROR("mail not exist, id = ~p", [MailID]),
      not_exist;
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("get_attach_mail_by_id error, reason = ~p", [Reason]),
      {fail, "HintSystemError"}
  end.

get_attach_mail(Uin) when is_integer(Uin) ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [get_attach_mail_by_uin, Uin]) of
    {success, AttachMailList} -> {success, AttachMailList};
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("get_attach_mail error, reason =~p", [Reason]),
     {fail, "HintSystemError"}
  end.

filter_mail_from_db(Uin,MailType) ->
  DBNode = mail_util:get_database_node(),
  case MailType of
    1 ->
      case rpc:call(DBNode, database_monitor, execute,[filter_friend_mail_by_uin, Uin]) of
        success -> success;
        fail ->
          ?FILE_LOG_ERROR("filter_friend_mail_by_uin error", []),
          throw({custom,"filter_friend_mail_by_uin error"})                %获取好友邮件失败则返回空列表
      end;
    2 ->
      case rpc:call(DBNode, database_monitor, execute,[filter_system_mail_by_uin, Uin]) of
        success -> success;
        fail ->
          ?FILE_LOG_ERROR("filter_system_mail_by_uin error", []),
          throw({custom,"filter_system_mail_by_uin error"})
      end
end.

filter_mail_overtime() ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [filter_mail_overtime, 0]) of
    success -> success;
    fail ->
      ?FILE_LOG_DEBUG("get_bulletin_mail_by_id error", []),
      {fail, "HintSystemError"}
  end.

get_bulletin_mail_by_id(MailID) when is_integer(MailID) ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [get_bulletin_mail, MailID]) of
    {success, BulletinMail} -> {success, BulletinMail};
    not_exist ->
      ?FILE_LOG_ERROR("mail not exist, id = ~p", [MailID]),
      not_exist;
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("get_bulletin_mail_by_id error, reason = ~p", [Reason]),
      {fail, "HintSystemError"}
  end.

get_bulletin_mail() ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [load_bulletin_mail, 0]) of
    {success, BulletinMailList} -> {success, BulletinMailList};
    {fail, Reason} ->
      ?FILE_LOG_DEBUG("get_bulletin_mail error, reason = ~p", [Reason]),
      {fail, "HintSystemError"}
  end.

insert_bulletin_mail(BulletinMail) when is_record(BulletinMail, bulletin_mail) ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [insert_bulletin_mail, BulletinMail]) of
    {success, MailID} -> {success, BulletinMail#bulletin_mail{mail_id = MailID}};
    fail ->
      ?FILE_LOG_DEBUG("insert bulletin mail error, mail = ~p",[BulletinMail]),
      {fail, "HintSystemError"}
  end.

insert_attach_mail(AttachMail) when is_record(AttachMail, attach_mail) ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [insert_attach_mail, AttachMail]) of
    {success, MailID} ->
      Uin = AttachMail#attach_mail.mail_dest,
      MailType = AttachMail#attach_mail.mail_type, %1。好友邮件 2. 系统邮件
      mail_work:filter_mail(Uin, MailType),
      {success, AttachMail#attach_mail{mail_id = MailID}};
    fail ->
      ?FILE_LOG_DEBUG("insert attach mail error, mail = ~p", [AttachMail]),
      {fail, "HintSystemError"}
  end.

del_bulletin_mail(MailID) when is_integer(MailID) ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [delete_bulletin_mail, MailID]) of
    success -> success;
    fail ->
      ?FILE_LOG_ERROR("mail_db: delete bulletin mail error! id = ~p", [MailID]),
      fail
  end.

del_attach_mail(MailID) when is_integer(MailID) ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [delete_attach_mail, MailID]) of
    success -> success;
    fail ->
      ?FILE_LOG_ERROR("mail_db: delete_attach_mail error! id = ~p", [MailID]),
      fail
  end.

del_mass_attach_mail(MailIDList) when is_list(MailIDList) ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [delete_mass_attach_mail, MailIDList]) of
    success -> success;
    Other ->
      ?FILE_LOG_ERROR("mail_db:delete mass attach_mail error, idlist = ~p, reason = ~p", [MailIDList, Other]),
      fail
  end.

get_all_user()  ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [get_all_user, -1]) of
    {success, Result}->{success, Result};
    fail ->
      ?FILE_LOG_ERROR("mail_db: get_all_user error! ", []),
      fail
  end.

