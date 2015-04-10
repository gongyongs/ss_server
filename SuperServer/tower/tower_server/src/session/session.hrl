%% return string()
-define(SESSION_VERSION, element(2, application:get_key(session, vsn))).
-define(SESSION_DESCRIPTION, element(2, application:get_key(session, description))).
