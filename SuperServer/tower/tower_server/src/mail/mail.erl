-module(mail).
-author('erlangonrails@gmail.com').
-export([start/0, stop/0]).

-include("mail.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-include("../csv.hrl").
-export([
  present_friend_strength/2,
  get_mail/1,
  add_attach_mail/6,
  send_mass_attach_mail/6,
  add_bulletin_mail/3,
  add_mail_template/4,
  get_attach_mail/1,
  del_attach_mail/1,
  get_bulletin_mail/0,
  get_bulletin_mail/1,
  del_bulletin_mail/1,
  server_rank_reward/3,
  friend_rank_reward/2,
  get_mail_template/0,   %获取模板
  del_mail_template/1,
  get_bulletin/0,
  send_feedback_mail/2,
  get_attach_mail_by_uin/1,
  delete_mass_attach_mail/1
]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(mail).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(mail).


present_friend_strength(SourceUin, DestUin) when is_integer(SourceUin) andalso is_integer(DestUin) ->
  ProcName = mail_sup:hash_uin_to_proc(DestUin),
  mail_work:execute(ProcName, present_friend_strength, {SourceUin, DestUin}).

get_mail(Uin) when is_integer(Uin) ->
  ProcName = mail_sup:hash_uin_to_proc(Uin),
  mail_work:execute(ProcName, get_mail, Uin).

get_bulletin() ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, get_assemble_bulletin_mail, 1).

send_mass_attach_mail(Sid, DestUinList, MailTemplateID, MailParamList, Attach, Term) ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, send_mass_attach_mail, {Sid, DestUinList, MailTemplateID, MailParamList, Attach, Term}).

add_attach_mail(Sid, Did, MailTemplateID, MailParamList, Attach, Term) ->
  ProcName = mail_sup:hash_uin_to_proc(Did),
  mail_work:execute(ProcName, add_attach_mail, {Sid, Did, MailTemplateID, MailParamList, Attach, Term}).

add_bulletin_mail(MailTemplateID, MailParamList, Term) ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, add_bulletin_mail, {MailTemplateID, MailParamList, Term}).

add_mail_template(Tag, Title, Content, ParamLen) ->
  mail_template:add_mail_template(Tag, Title, Content, ParamLen).

get_attach_mail(MailID) ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, get_attach_mail, MailID).

del_attach_mail(MailID) ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, del_attach_mail, MailID).

get_bulletin_mail(MailID) ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, get_bulletin_mail, MailID).

del_bulletin_mail(MailID) ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, del_bulletin_mail, MailID).




server_rank_reward(Uin, RankPercent, Rank) ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, server_rank_reward, {Uin, RankPercent, Rank}).

friend_rank_reward(Uin, Rank) ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, friend_rank_reward, {Uin, Rank}).

send_feedback_mail(FeedBackData,Uin) ->
  ?FILE_LOG_DEBUG("mail.erl=>send_feedback_mail",[]),
  mail_work:send_feedback_mail_cast(FeedBackData,Uin).

get_attach_mail_by_uin(Uin) ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, get_attach_mail_by_uin, Uin).

delete_mass_attach_mail(MailIDList) ->
  ProcName = mail_sup:get_random_proc(),
  mail_work:execute(ProcName, delete_mass_attach_mail, MailIDList).


get_mail_template() ->
  mail_template:get_mail_template().

get_bulletin_mail()->
  mail_db:get_bulletin_mail().

del_mail_template(TemplateID)->
  mail_template:del_mail_template(TemplateID).

