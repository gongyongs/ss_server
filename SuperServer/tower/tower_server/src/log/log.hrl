%% return string()
-define(LOG_VERSION, element(2, application:get_key(log, vsn))).
-define(LOG_DESCRIPTION, element(2, application:get_key(log, description))).

-define(LOG_STAT_TIME, {4,0,0}).
%%单次读取的数据条数
-define(DB_SINGLE_MAX_READ, 500).
%%同时进行计算的进程数
-define(MAX_PROCESS_COUNT, 50).

-record(log_table, {
  name::string(),
  history_table_name::[],
  latest_create_ts::integer(),
  cur_table_name::string(),
  roll_cycle::integer()       %%滚动周期，以天为单位
}).

-define(TABLE_LIST, ["MoneyFlow", "GameEndFlow", "Lottery", "Login", "Register", "RewardFlow", "PaySuccessFlow",
  "PayFailFlow", "ConsumeFlow", "StrengthenFlow", "MissionFlow", "AchievementFlow", "OperationTimeFlow", "CNPaySuccessFlow", "CNPayFailFlow"]).
-define(MONEY_FLOW_CREATE_SQL,
  "create table if not exists ~s (
    EventTime      datetime not null,
    TimeKey        varchar(32) not null,
    Uin            int(11)  not null,
    FlowType       char(64)  ,
    MoneyType      int(11)  default 0,
    MoneyBefore    int(11)  default 0,
    MoneyAfter     int(11)  default 0,
    Addition         varchar(1024) default '0',
    primary key(TimeKey)
)ENGINE = InnoDB CHARSET=utf8;").
-define(MONEY_FLOW_DB_TABLE_UPDATE_TIME, 30).  %30天

-define(GAME_END_CREATE_SQL,
"create table if not exists ~s (
    EventTime           datetime not null,
    TimeKey             varchar(32) not null,
    Uin                 int(11)  not null,
    GameTime            int(11)  not null,
    GameResult          int(11)  not null,
    TollgateID          int(11)  not null,
    TollgateType        int(11)	 not null,
    GainGold            int(11)	 not null,
    GainStar            int(11)	 not null,
    GainScore           int(11)  not null,
    EndlessCount        int(11)	 not null,
    UseProp             varchar(10240),
    GainDrop            varchar(10240),
    Addition         varchar(1024) default '0',
    primary key(TimeKey)
)ENGINE = InnoDB CHARSET=utf8;").
-define(GAME_END_DB_TABLE_UPDATE_TIME, 30).  %%2天

-define(LOTTERY_CREATE_SQL,
  "create table if not exists ~s (
	EventTime          datetime not null,
	TimeKey            varchar(32) not null,
	Uin                int(11)  not null,
	LotteryType        int(11)  not null,
	CostType           int(11)  not null,
	CostCount          int(11)  not null,
	GainDrop           varchar(10240)  not null,
	Addition         varchar(1024) default '0',
	primary key(TimeKey)
)ENGINE = InnoDB CHARSET=utf8;").
-define(LOTTERY_DB_TABLE_UPDATE_TIME, 30).

-define(LOGIN_CREATE_SQL,
"create table if not exists ~s (
	EventTime          datetime not null,
	TimeKey            varchar(32) not null,
	Uin                int(11)  not null,
	PlatType           int(11)  not null,
	LoginTime          int(11)  not null,
	Device             varchar(256) not null,
	Ip                 char(32) not null,
	Addition         varchar(1024) default '0',
	primary key(TimeKey)
)ENGINE = InnoDB CHARSET=utf8;").
-define(LOGIN_DB_TABLE_UPDATE_TIME, 30).

-define(REGISTER_CREATE_SQL,
"create table if not exists ~s (
	EventTime        datetime  not null,
	TimeKey          varchar(32) not null,
	Uin              int(11)   not null,
	RegisterTime     int(11)   not null,
	PlatType         int(11)   not null,
	PlatID           varchar(256),
	PlatDisName      varchar(256),
	Device           varchar(256) not null,
	Ip               char(32) not null,
	Addition         varchar(1024) default '0',
	primary key(TimeKey)
)ENGINE = InnoDB CHARSET=utf8;").
-define(REGISTER_DB_TABLE_UPDATE_TIME, 30).

-define(REWARD_CREATE_SQL,
"create table if not exists ~s (
	EventTime        datetime      not null,
	TimeKey          varchar(32) not null,
	Uin              int(11)       not null,
	RewardType       int(11)	     not null,
	GetRewardTime    int(11)       not null,
	RewardItemType   int(11)       not null,
	RewardItemID     varchar(256),
	RewardItemCount  int(11)       not null,
	RewardDesc       varchar(1024),
	Addition         varchar(1024) default '0',
	primary key(TimeKey)
)ENGINE = InnoDB CHARSET=utf8;
").
-define(REWARD_DB_TABLE_UPDATE_TIME, 30).

-define(PAY_CREATE_SQL,
"create table if not exists ~s (
	EventTime       datetime        not null,
	TimeKey         varchar(32) not null,
	Uin             int(11)         not null,
	Plat            char(32)        not null,
	OrderID         varchar(128)    not null,
	PayTime         int(11)         not null,
	PayItemID       varchar(256)    not null,
	BuyItemType     int(11),
	BuyItemID       int(11),
	BuyCount        int(11),
	BuyDesc         varchar(1024),
	BuyBackup       varchar(1024),
	Addition        varchar(1024) default '0',
	primary key(TimeKey)
)ENGINE = InnoDB CHARSET=utf8;").
-define(PAY_DB_TABLE_UPDATE_TIME, 30).

-define(PAY_FAIL_CREATE_SQL,
  "create table if not exists ~s (
    EventTime       datetime        not null,
    TimeKey         varchar(32) not null,
    Uin             int(11)         not null,
    Plat            char(32)        not null,
    OrderID         varchar(128),
    PayTime         int(11)         not null,
    PayInfo         blob            not null,
    Reason          varchar(1024)   not null,
    Addition        varchar(256) default '0',
    primary key(TimeKey)
  )ENGINE = InnoDB CHARSET=utf8;").

-define(CONSUME_CREATE_SQL,
"create table if not exists ~s (
	EventTime       datetime        not null,
	TimeKey         varchar(32) not null,
	Uin             int(11)         not null,
	BuyTime         int(11)         not null,
	BuyItemType     int(11)         not null,
	CommodityID     varchar(128)    not null,
	BuyItemID       varchar(256),
	BuyItemCount    int(11)         not null,
	CostType        int(11)         not null,
	CostCount       int(11)         not null,
	BuyDesc         varchar(1024),
	Addition        varchar(1024) default '0',
	primary key(TimeKey)
)ENGINE = InnoDB CHARSET=utf8;").
-define(CONSUME_DB_TABLE_UPDATE_TIME, 30).

-define(STRENGTHEN_CREATE_SQL,
"create table if not exists ~s (
	EventTime           datetime        not null,
	TimeKey             varchar(32) not null,
	Uin                 int(11)         not null,
	StrengthenType      int(11)         not null,
	StrengthenObjID     varchar(256)    not null,
	StrengthenObjNo     varchar(256),
	ConsumeGoldCount    int(11)         not null,
	Swallow             varchar(10240)  not null,
	BeforeObjNo         varchar(256)    not null,
	BeforeObjExp        int(11)         not null,
	AfterObjNo          varchar(256)    not null,
	AfterObjExp         int(11)         not null,
	Addition            varchar(1024)   default '0',
	primary key(TimeKey)
)ENGINE = InnoDB CHARSET=utf8;").
-define(STRENGTHEN_DB_TABLE_UPDATE_TIME, 30).

-define(OPERATION_TIME_CREATE_SQL,
  "create table if not exists ~s (
    EventTime       datetime        not null,
    TimeKey         varchar(32) not null,
    Uin             int(11)         not null,
    Date            datetime        not null,
    TotalOperTime   int(11)         not null,
    Addition        varchar(1024)   default '0',
    primary key(TimeKey)
  )ENGINE = InnoDB CHARSET=utf8;").

-define(OPERATION_TIME_DB_TABLE_UPDATE_TIME, 30).

-define(MISSION_CREATE_SQL,
  "create table if not exists ~s (
    EventTime       datetime        not null,
    TimeKey         varchar(32) not null,
    Uin             int(11)         not null,
    MissionType     int(11)         not null,
    MissionID       varchar(256)    not null,
    MissionDesc     blob,
    MissionStatus   int(11)         not null,
    Addition        varchar(1024) default '0',
    primary key(TimeKey)
  )ENGINE = InnoDB CHARSET=utf8;").
-define(MISSION_DB_TABLE_UPDATE_TIME, 30).

-define(ACHIEVEMENT_CREATE_SQL,
  "create table if not exists ~s (
    EventTime            datetime        not null,
    TimeKey              varchar(32)     not null,
    Uin                  int(11)         not null,
    AchievementType      int(11)         not null,
    AchievementID        varchar(256)    not null,
    AchievementDesc      blob,
    AchievementStatus    int(11)         not null,
    Addition             varchar(1024) default '0',
    primary key(TimeKey)
  )ENGINE = InnoDB CHARSET=utf8;").
-define(ACHIEVEMENT_DB_TABLE_UPDATE_TIME, 30).

-define(CNPAY_SQL,
  "create table if not exists ~s (
    EventTime       datetime        not null,
    TimeKey         varchar(32)     not null,
    Uin             int(11)         not null,
    Plat            varchar(32)     not null,
    OrderID         varchar(128)    not null,
    BillNo          varchar(128)    not null,
    AccountID       varchar(128)    not null,
    GoodsID         varchar(128)    not null,
    GoodsPrice      varchar(32)     not null,
    CNCostCount     varchar(32)     not null,
    PayDesc         varchar(1024),
    Addition        varchar(1024) default '0',
    primary key(TimeKey)
  )ENGINE = InnoDB CHARSET=utf8;").
-define(CN_PAY_TABLE_UPDATE_TIME, 30).


