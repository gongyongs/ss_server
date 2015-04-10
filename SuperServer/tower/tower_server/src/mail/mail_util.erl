%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2014 19:03
%%%-------------------------------------------------------------------
-module(mail_util).
-author("Administrator").
-include("../../deps/file_log/include/file_log.hrl").
-include("mail.hrl").
-include("../dd_ms.hrl").

%% API
-export([
  get_cache_node/1,
  get_database_node/0,
  get_source_info/1
]).

-export([
  assemble_attach_mail/1,
  assemble_bulletin_mail/1
]).


get_cache_node(Index) when is_integer(Index)->
  {success, CacheHashRule} = dd_ms:read_config(cache_hash_rule),
  case hash_service_util:find_key_store_node(dd_util:to_list(Index), CacheHashRule) of
    fail ->
      ?FILE_LOG_ERROR("no available data node, ~p", [Index]),
      throw({custom, "HintSystemError"});
    {success, Node} when is_atom(Node) -> {success, Node}
  end.

get_database_node() ->
  {success, DBNode} = dd_ms:read_config(database_node),
  DBNode.


assemble_attach_mail(AttachMailList) when is_list(AttachMailList) ->
  lists:map(
    fun(AttachMailItem) ->
      {success, {Title, Content}} = mail_template:make_mail(AttachMailItem#attach_mail.mail_template_id, AttachMailItem#attach_mail.mail_param_list),
      case get_source_info(AttachMailItem#attach_mail.mail_source) of
        {success, Info} ->
          {AttachMailItem#attach_mail.mail_id, Info, Title, Content, AttachMailItem#attach_mail.mail_attachment, AttachMailItem#attach_mail.mail_add_ts, AttachMailItem#attach_mail.mail_type};
        {fail, Reason} ->
          ?FILE_LOG_ERROR("get source uin info error, reason = ~p", [Reason]),
          throw({custom, "logic error"})
      end
    end, AttachMailList).

assemble_bulletin_mail(BulletinMailList) when is_list(BulletinMailList) ->
  lists:map(
    fun(BulletinItem) ->
      {success, {Title, Content}} = mail_template:make_mail(BulletinItem#bulletin_mail.mail_template_id, BulletinItem#bulletin_mail.mail_param_list),
      {BulletinItem#bulletin_mail.mail_id, Title, Content, BulletinItem#bulletin_mail.mail_add_ts}
    end, BulletinMailList).

get_source_info(SrcUin) when is_integer(SrcUin) ->
  DbNode = get_database_node(),
  case SrcUin of
    -1 -> {success, {-1, "", "System"}};
    _ ->
      case rpc:call(DbNode, database_monitor, execute, [get_user_basic_info, SrcUin]) of
           {success, Info} -> {success, Info};
           {fail, Reason} -> {fail, Reason}
      end
  end.
