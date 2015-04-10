%% return string()
-define(DISPATCH_VERSION, element(2, application:get_key(dispatch, vsn))).
-define(DISPATCH_DESCRIPTION, element(2, application:get_key(dispatch, description))).

-define(POST, 'POST').
-define(GET, 'GET').

-record(version, {
  version_id::string(),
  version_package_url::string(),
  version_update_url::string()
}).

-record(login_user_info, {
  uin::integer(),
  ip::string(),
  device::string(),
  plat_type::string(),
  plat_dis_name::string(),
  plat_id::string()
}).