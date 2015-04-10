-module(gateway_db).
-author("j").
-include("../../deps/file_log/include/file_log.hrl").
-include("../dd_ms.hrl").
-include("gateway.hrl").
%% API
-export([
  get_model_function_state/1,
  get_closed_model/0,
  get_notice_rd/1 %获取公告
]).

get_model_function_state(Model_id)when is_list(Model_id)->
DBNode = gateway_util:get_database_node(),
  case rpc:call(DBNode,database_monitor,execute,[get_model_state, Model_id]) of
    {success, Number}->Number;
    {fail,Reason} ->
      ?FILE_LOG_ERROR("gateway_db: get_model_function_state error! ", []),
      {fail,Reason}
  end.

get_closed_model()->

  Model_state_List = [#model_state{model_id="endless",state_num=get_model_function_state("endless")},
   #model_state{model_id="reward",state_num=get_model_function_state("reward")},
   #model_state{model_id="treasure",state_num=get_model_function_state("treasure")},
   #model_state{model_id="upg",state_num=get_model_function_state("upg")},
   #model_state{model_id="eva",state_num=get_model_function_state("eva")},
   #model_state{model_id="mail",state_num=get_model_function_state("mail")},
   #model_state{model_id="shop",state_num=get_model_function_state("shop")}
   ],
  Closed_model_List = lists:filter(fun(X)->X#model_state.state_num =:= 0 end,Model_state_List),
  Closed_model_id_List = lists:map(fun(X)-> X#model_state.model_id end,Closed_model_List),
  {success,Closed_model_id_List}.

get_notice_rd(Uin) ->
  DBNode = gateway_util:get_database_node(),
  case rpc:call(DBNode,database_monitor,execute,[get_notice_rd,Uin]) of
    {success, NoticeList}-> {success, NoticeList};
    {fail,Reason} ->{fail,Reason}
  end.
