create database if not exists tower_login;
use tower_login;
create table if not exists login(
uin int(11) unsigned not null,
device varchar(128),
uname varchar(128),
dis_name varchar(128),
platform_info blob,
addition blob,
create_ts int(11) unsigned,
primary key(uin),
unique index uname_index (uname,device)
)ENGINE = InnoDB  CHARSET=utf8;

create table if not exists uin(
uin int(11) unsigned not null default '0'
)ENGINE = InnoDB  CHARSET=utf8;
insert into uin values(1);

create table if not exists version(
version_id 	varchar(128) not null,
version_url varchar(1024),
package_url varchar(1024),
primary key(version_id)	
)ENGINE = InnoDB CHARSET=utf8;

