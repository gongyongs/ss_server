%% return string()
-define(DATABASE_VERSION, element(2, application:get_key(database, vsn))).
-define(DATABASE_DESCRIPTION, element(2, application:get_key(database, description))).
