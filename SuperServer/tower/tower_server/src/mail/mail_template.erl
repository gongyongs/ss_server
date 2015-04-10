%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 九月 2014 上午10:16
%%%-------------------------------------------------------------------
-module(mail_template).
-author("zqlt").

-behaviour(gen_server).
-include("mail.hrl").
-include("../../deps/file_log/include/file_log.hrl").
 -include("../dd_ms.hrl").
-include("../../deps/mysql/include/mysql.hrl").
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
-define(PLACE, <<"#">>).
-define(REPLACE, <<"~s">>).
-define(PLACE_VALUE, $#).
-record(state, {}).

-include("../csv.hrl").

-export([
  add_mail_template/4,
  del_mail_template/1,
  get_mail_template/0
]).



-export([
  make_mail/2,
  get_template_by_tag/1,
  load_mail_template_config_by_id/1,
  verify_mail_template/2
]).

%%%===================================================================
%%% API
%%%===================================================================

verify_mail_template(TemplateId, ParamList) ->
  ParamLen = length(ParamList),
  ?FILE_LOG_DEBUG("mail_template=>verify_mail_template=>", []),
  case ets:lookup(mail_template, TemplateId) of
    [] -> {fail, "template id not exist"};
    [#template_mail{template_content_parm_len = ParamLen} = Template] -> {success, Template};
    [#template_mail{template_content_parm_len = _}] -> {fail, "param not enough"}
  end.

make_mail(TemplateId, ParamList) ->
  case verify_mail_template(TemplateId, ParamList) of
    {fail, Reason} -> {fail, Reason};
    {success, Template} ->
      BFormat = binary:replace(list_to_binary(Template#template_mail.template_content), ?PLACE, ?REPLACE, [global]),
      LFormat = binary_to_list(BFormat),
      case format_mail_content(LFormat, ParamList) of
        {success, MailContent} -> {success, {Template#template_mail.template_title, MailContent}};
        fail -> fail
      end
  end.

format_mail_content(Format, Param) ->
  try
    Param2 = [dd_util:to_list(V) || V <- Param],
    {success, lists:flatten(io_lib:format(Format, Param2))}
  catch
    What:Type ->
      ?FILE_LOG_ERROR("what=~p, type=~p, statck=~p", [What, Type, erlang:get_stacktrace()]),
      fail
  end.

add_mail_template(Tag, Title, Content, ParamLen) ->
  case param_count_check(Content, ?PLACE_VALUE, 0) of
    ParamLen ->
      Tid = dd_util:to_list(dd_util:timestamp()),
      MailTemplate = #template_mail{template_id = Tid, template_tag = Tag, template_title = Title, template_content = Content, template_content_parm_len = ParamLen, template_type = 2},
      gen_server:call(?MODULE, {add_mail_template, [MailTemplate,Tid]});
    Other ->
      ?FILE_LOG_WARNING("param count=~p", [Other]),
      fail
  end.

get_mail_template() ->
  DBNode = mail_util:get_database_node(),
  DBList =
    case rpc:call(DBNode, database_monitor, execute, [load_template_mail, 0]) of
      {success, List} -> List;
      {fail, Reason1} ->
        ?FILE_LOG_ERROR("load template mail from db error: ~p", [Reason1]),
        []
    end,
  {success, DBList}.

get_template_by_tag(Tag) when is_list(Tag) ->
  get_template_by_tag_1(ets:tab2list(mail_template), Tag).
get_template_by_tag_1([], _Tag) -> fail;
get_template_by_tag_1([Template | T], Tag) ->
  case Template#template_mail.template_tag of
    Tag -> {success, Template};
    _ -> get_template_by_tag_1(T, Tag)
  end.
param_count_check([], _, Count) -> Count;
param_count_check([V|T], V, Count) -> param_count_check(T, V, Count+1);
param_count_check([_|T], V, Count) -> param_count_check(T, V, Count).


%% reload_mail_template() ->
%%   gen_server:call(?MODULE, reload_mail_template).


del_mail_template(TemplateId) ->
  gen_server:call(?MODULE, {del_mail_template, TemplateId}).

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
  ets:new(mail_template, [set, protected, named_table, {keypos, #template_mail.template_id}]),
  ets:new(mail_template_config, [set, protected, named_table, {keypos, #res_template_mail_config.id}]),
  erlang:start_timer(1*30000, self(), 'start_load_config'),
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
handle_call({add_mail_template, [MailTemplate,Tid]}, _From, State) ->
  case db_add_mail_template(MailTemplate) of
    success ->
      ets:insert(mail_template, MailTemplate),
      {reply, {success,Tid}, State};            %返回{success,template_id}
    fail -> {reply, fail, State}
  end;
handle_call({del_mail_template, TemplateId}, _From, State) ->
  case db_del_mail_template(TemplateId) of
    success ->
      ets:delete(mail_template, TemplateId),
      {reply, success, State};
    fail -> {reply, fail, State}
  end;
handle_call(reload_mail_template, _From, State) ->
  ets:delete_all_objects(mail_template),
  {success, TemplateList} = load_mail_template(),
  lists:foreach(
    fun(MailTemplate) ->
      ets:insert(mail_template, MailTemplate)
    end, TemplateList),
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
  {success, TemplateList} = load_mail_template(),
  %%?FILE_LOG_DEBUG("list = ~p",[TemplateList]),
  lists:foreach(
    fun(MailTemplate) ->
      ets:insert(mail_template, MailTemplate)
    end, TemplateList),
  ?FILE_LOG_DEBUG("FINISH load mail config", []),
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

load_mail_template() ->
  {success, CacheNode} = mail_util:get_cache_node(1),
  ConfigList =
    case rpc:call(CacheNode, cache, load_mail_template_config, []) of
      {success, TemplateList} ->
        lists:map(
          fun(Item) ->
            ets:insert(mail_template_config, Item),
            #template_mail{template_id = Item#res_template_mail_config.id, template_type = Item#res_template_mail_config.type,
              template_tag = Item#res_template_mail_config.tag, template_title = Item#res_template_mail_config.title, template_content = Item#res_template_mail_config.content,
              template_content_parm_len = param_count_check(Item#res_template_mail_config.content, ?PLACE_VALUE, 0)}
          end, TemplateList);
      {fail, Reason} ->
        ?FILE_LOG_ERROR("load template mail from config error: ~p", [Reason]),
        []
    end,
  DBNode = mail_util:get_database_node(),
  DBList =
    case rpc:call(DBNode, database_monitor, execute, [load_template_mail, 0]) of
      {success, List} -> List;
      {fail, Reason1} ->
        ?FILE_LOG_ERROR("load template mail from db error: ~p", [Reason1]),
        []
    end,
  {success, lists:merge(ConfigList, DBList)}.    %将两个列表合并为一个列表

load_mail_template_config_by_id(TemplateID) ->
  case ets:lookup(mail_template_config, TemplateID) of
    [Item] -> {success, Item};
    {fail, Reason} ->
      ?FILE_LOG_ERROR("load template mail from config error: ~p", [Reason]),
      {fail, Reason}
  end.



db_add_mail_template(TemplateMail) ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [insert_template_mail, TemplateMail]) of
    success -> success;
    fail ->
      ?FILE_LOG_ERROR("db_add_mail_template error", []),
      fail
  end.

db_del_mail_template(TemplateId) ->
  DBNode = mail_util:get_database_node(),
  case rpc:call(DBNode, database_monitor, execute, [delete_template_mail, TemplateId]) of
    success -> success;
    fail ->
      ?FILE_LOG_ERROR("db_add_mail_template error", []),
      fail
  end.


