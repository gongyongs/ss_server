create database if not exists tower;

use tower;

create table if not exists account(
uin int(11) unsigned not null,
gold_coin int(11) unsigned not null,
gem int(11) unsigned not null,
platform_info blob,
hero blob,
backpack blob, 
mission blob,
achievement blob,
shop blob,
stage blob,
login_reward blob,
strength blob,
lottery blob,
guild blob,
competitive blob,
addition blob,
create_ts int unsigned,
last_login_ts int unsigned,
primary key(uin)
)ENGINE=InnoDB  CHARSET=utf8;

create table if not exists uuid(
id int(11) unsigned not null default '0',
uuid int(11) unsigned not null default '1',
primary key(id)
)ENGINE=InnoDB  CHARSET=utf8;

create table if not exists mail_id (freeid int(11) unsigned not null default '0') ENGINE=InnoDB  CHARSET=utf8;
insert into mail_id values(1);

create table if not exists attach_mail (
	mail_id int(11) unsigned not null,
	mail_source int(11) not null,
	mail_dest int(11) not null,
	mail_template_id varchar(128) not null,
	mail_param blob,
	mail_attachment blob,	
	mail_add_time int(11) unsigned not null,
	mail_type int(11) unsigned not null,
	mail_term int(11) unsigned not null,
	primary key(mail_id)
)  ENGINE=InnoDB CHARSET=utf8; 

create table if not exists bulletin_mail (
	mail_id int(11) unsigned not null,
	mail_template_id varchar(128) not null,
	mail_param blob,
	mail_add_time int(11) unsigned not null,
	mail_term int(11) unsigned not null,
	primary key(mail_id)
) ENGINE=InnoDB CHARSET=utf8;

create table if not exists mail_template (
	template_id varchar(128) not null,
	template_type int(11) unsigned not null,	
	template_tag varchar(128) not null,	
	template_title varchar(128),
	template_content varchar(1024) not null,
	template_content_param_len smallint not null,
	unique index tmpl(template_tag),
	PRIMARY KEY (template_id)	
)  ENGINE=InnoDB CHARSET=utf8;
insert into mail_template values('default',2,'default','Notice','#',1);

create table if not exists shop_config(
	id char(32) not null,
	name varchar(128) not null,
	image varchar(128) not null,
	goods_id varchar(128),
	goods_type int(11) unsigned not null,
	goods_count int unsigned not null,
	cost_type int(11) unsigned not null,
	cost_count char(32) not null,
	gift_type int(11) unsigned not null,
	gift_count int(11) unsigned not null,
	restriction int(11) unsigned not null,
	recommended int(11) unsigned not null,
	start_time int unsigned,
	over_time int unsigned,
	primary key(id)
)ENGINE=InnoDB CHARSET=utf8;

create table if not exists charge ( 
transaction_id char(64) not null,
uin int(11) unsigned not null,
transaction_ts int(11) unsigned,
unique index index_tran_id (transaction_id, uin)
) ENGINE=InnoDB  CHARSET=utf8;

create table if not exists model_state(
	model_id char(32) not null,
	state_num  int(11) unsigned not null,
	primary key(model_id)
)ENGINE=InnoDB CHARSET=utf8;

create table if not exists lottery_discount(
	id int(11) unsigned not null,
	tool_id char(32) not null,
	price int(11) unsigned not null,
	is_discount int(11) unsigned not null,
	start_time  int unsigned not null,
	stop_time  int unsigned not null,
	primary key(id)
)ENGINE=InnoDB CHARSET=utf8;

create table if not exists notice_rd (
	notice_id int(11) unsigned not null auto_increment,
	notice_title varchar(256) not null,
	notice_date varchar(128) not null,
	notice_detail text not null,
	notice_pic_url varchar(1024) not null,
	notice_sign varchar(1024),
	primary key(notice_id)
) ENGINE=InnoDB auto_increment=1 CHARSET=utf8;

