%% return string()
-define(GATEWAY_VERSION, element(2, application:get_key(gateway, vsn))).
-define(GATEWAY_DESCRIPTION, element(2, application:get_key(gateway, description))).

-define(POST, 'POST').

-define(GET, 'GET').

-record(model_state,{
  model_id::string(),        %模块ID
  state_num::integer()       %模块状态
}).


-define(SIGNATURE, "!1AQaq#3").




