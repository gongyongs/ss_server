%% return string()
-define(SYS_LOG_VERSION, element(2, application:get_key(sys_log, vsn))).
-define(SYS_LOG_DESCRIPTION, element(2, application:get_key(sys_log, description))).


%%api
-define(SYS_LOG(Param),sys_log_server:write_sys_log(Param)).

-define(ADMIN_LOG(Param),sys_log_server:write_admin_log(Param)).

-define(PLAYER_LOG(Param),sys_log_server:write_player_log(Param)).

-define(COM_LOG(Format,Param),sys_log_server:write(?MODULE,?LINE,Format,Param)).   %%可替代file_log的功能file_log_info("",[])

-define(ERROR_LOG(Format,Param),sys_log_server:write_error_log(?MODULE,?LINE,Format,Param)).   %%可替代file_log的功能file_log_error("",[])