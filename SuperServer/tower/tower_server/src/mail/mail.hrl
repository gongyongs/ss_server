%% return string()
-define(MAIL_VERSION, element(2, application:get_key(mail, vsn))).
-define(MAIL_DESCRIPTION, element(2, application:get_key(mail, description))).

-record(mail_attachment,{
  type::integer(),        %%附件类型: 1=金币 2=钻石 3=体力 4=道具
  property_id::string(),  %%道具id
  count::integer()        %%附件数量
}).

-record(template_mail,{
  template_id::string(),
  template_type::integer(),   %%自动填充参数，或者手动填充
  template_tag::string(),     %%邮件唯一标示
  template_title::string(),
  template_content::string(),
  template_content_parm_len::integer()
}).

-record(attach_mail,{
  mail_id::integer(),
  mail_source::integer(),
  mail_dest::integer(),
  mail_template_id::string(),
  mail_param_list::[],
  mail_attachment::#mail_attachment{},
  mail_add_ts::integer(),
  mail_type::integer(), %%好友邮件或者系统邮件， 1。好友邮件 2. 系统邮件
  mail_term::integer()
}).

-record(bulletin_mail,{
  mail_id::integer(),
  mail_template_id::string(),
  mail_param_list::[],
  mail_add_ts::integer(),
  mail_term::integer()
}).


%%email feedback
-record(email, {
  server_ip   , % (必填) 邮件服务器ip(如: "smtp.qq.com")
  account     , % (必填) 你自己的邮箱名(如: "281754179@qq.com")
  password    , % (必填) 密码
  to_emails   , % (必填) 要发往的邮箱(格式:["","",...])(如: ["281754179@qq.com", "leptune@live.cn"])
  server_port , % (可选) 要传整数。如ssl为true，则默认端口为465, 否则默认端口为25，也可以手动指定
  ssl = true  , % (可选) 是否需要ssl加密(true 或 false)
  subject = "", % (可选) 邮件标题
  text         % (可选) (正文)将文本内容发往邮箱，text的值为存放该内容的文件路径(路径以'/'分隔，不要以'\\'分隔)
}).

-record(socket, {type, sock}).

-define(SSL_SERV_PORT_DEF, 465).
-define(NOT_SSL_SERV_PORT_DEF, 25).


-define(SMTP_SERVER_IP, "smtp.exmail.qq.com").
-define(SEND_FEEDBACK_ACCOUNT, "support@ogresugar.com").
-define(SEND_FEEDBACK_PASSWORD, "Longtugame1").
-define(SEND_TO_ACCOUNT, "support@ogresugar.com").
