%% return string()
-define(HTTP_PROC_VERSION, element(2, application:get_key(http_proc, vsn))).
-define(HTTP_PROC_DESCRIPTION, element(2, application:get_key(http_proc, description))).

-define(POST, 'POST').

-define(GET, 'GET').


-define(APP_ID_INVALID,       "AppIDInvalid").
-define(SIGNATURE_INVALID,    "SignatureInvalid").
-define(ORDER_COMPLETED,      "OrderHasCompleted").
-define(USER_PAY_FAILED,      "UserPlatformPayFailed").
-define(USER_PAY_TIMEOUT,     "UserPlatformPayTimeOut").
-define(GOOD_PRICE_INVALID,   "GoodsPriceInvalid").
-define(PAY_DEBUG_MODE,       "OrderIsDebugMode").
-define(LOGICAL_ERROR,        "LogicalError").
-define(EXCEPTION,            "Exception").


-define(SUCCESS, "OrderSuccess").

