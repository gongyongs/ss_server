%% return string()
-define(MASTER_VERSION, element(2, application:get_key(master, vsn))).
-define(MASTER_DESCRIPTION, element(2, application:get_key(master, description))).
