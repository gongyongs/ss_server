%% return string()
-define(TOKEN_VERSION, element(2, application:get_key(token, vsn))).
-define(TOKEN_DESCRIPTION, element(2, application:get_key(token, description))).

-record(token_rd, {uin, token, add_time, login_info}).

-define(TIMEOUT, 30 * 1000).
-define(TOKEN_EXPIRED_TIME, 3000).
