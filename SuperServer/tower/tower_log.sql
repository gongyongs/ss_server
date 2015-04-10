create database if not exists tower_log;

use tower_log;

create table if not exists TableName(
name char(32) not null,
history_names blob,
latest_name char(128) not null,
last_update_ts int(11) default 0,
roll_cycle int(11) not null,
primary key(name)
) ENGINE = InnoDB  CHARSET=utf8;