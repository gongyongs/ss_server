%% Log APIs:

-define(FILE_LOG_ERROR(Data, Arg), 
          file_log_server:send(error, self(), ?MODULE, ?LINE, Data, Arg)).
-define(FILE_LOG_WARNING(Data, Arg), 
          file_log_server:send(warning, self(), ?MODULE, ?LINE, Data, Arg)).
-define(FILE_LOG_INFO(Data, Arg), 
          file_log_server:send(info, self(), ?MODULE, ?LINE, Data, Arg)).
-define(FILE_LOG_DEBUG(Data, Arg), 
          file_log_server:send(debug, self(), ?MODULE, ?LINE, Data, Arg)).


%% 与Log Server交互的APIs:
%% 支持四个级别的日志 error | warning | info | debug
-define(FILE_LOG_GET_LEVEL, file_log_server:get()).
-define(FILE_LOG_SET_LEVEL(Level), file_log_server:set(Level)).
-define(FILE_LOG_ROTATE, file_log_server:rotate()).
-define(FILE_LOG_GET_ROTATE_INTERVAL, file_log_server:get_rotate_interval()).
