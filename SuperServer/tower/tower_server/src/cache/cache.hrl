%% return string()
-define(CACHE_VERSION, element(2, application:get_key(cache, vsn))).
-define(CACHE_DESCRIPTION, element(2, application:get_key(cache, description))).

-define(LOGIN_REWARD_FRESH_TIME, {3,0,0}).  %%每天凌晨三点

