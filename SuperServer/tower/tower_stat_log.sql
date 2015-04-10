create database if not exists tower_stat_log;

use tower_stat_log;

create table if not exists StatActivationRetention (
ID int auto_increment primary key,
Dat datetime not null,
ActUser int(11) unsigned not null,
RegUser int(11) unsigned not null,
OneDayRem int(11) unsigned not null,
TwoDayRem int(11) unsigned not null,
ThreeDayRem int(11) unsigned not null,
FourDayRem int(11) unsigned not null,
FiveDayRem int(11) unsigned not null,
SixDayRem int(11) unsigned not null,
SevenDayRem int(11) unsigned not null
)ENGINE=InnoDB AUTO_INCREMENT=1 CHARSET=utf8;

create table if not exists StatAtk (
ID int auto_increment primary key,
Dat datetime not null,
Atk varchar(128) not null,
UserCount int(11) unsigned not null
)ENGINE=InnoDB AUTO_INCREMENT=1 CHARSET=utf8;

create table if not exists StatTollgateDrain (
ID int auto_increment primary key,
Dat datetime not null,
TollgateId  int(11) unsigned not null,
DrainCount  int(11) unsigned not null
)ENGINE=InnoDB AUTO_INCREMENT=1 CHARSET=utf8;

create table if not exists StatBaseTollgate (
ID int auto_increment primary key,
Dat datetime not null,
TollgateID int(11) unsigned not null,
StartUserCount int(11) unsigned not null,
PlayTimes int(11) unsigned not null,
FinishTimes int(11) unsigned not null,
FinishRate  varchar(128) not null,
BombTimes  int(11) unsigned not null,
FreezeTimes int(11) unsigned not null,
FinishUserCount int(11) unsigned not null,
TowerCount int(11) unsigned not null,
OneStarCount int(11) unsigned not null,
TwoStarCount int(11) unsigned not null,
ThreeStarCount int(11) unsigned not null
)ENGINE=InnoDB  AUTO_INCREMENT=1 CHARSET=utf8;

create table if not exists StatEndlessTollgate (
ID int auto_increment primary key,
Dat datetime not null,
TollgateId  int(11) unsigned not null,
StartUserCount int(11) unsigned not null,
PlayTimes int(11) unsigned not null,
BombTimes int(11) unsigned not null,
FreezeTimes int(11) unsigned not null,
TowerCount int(11) unsigned not null
)ENGINE=InnoDB AUTO_INCREMENT=1 CHARSET=utf8;

create table if not exists StatEndlessTollgateAtk (
ID int auto_increment primary key,
Dat datetime not null,
WaveNum int(11) unsigned not null,
AtkStep1 int(11) unsigned not null,
AtkStep2 int(11) unsigned not null,
AtkStep3 int(11) unsigned not null,
AtkStep4 int(11) unsigned not null,
AtkStep5 int(11) unsigned not null,
AtkStep6 int(11) unsigned not null,
AtkStep7 int(11) unsigned not null,
AtkStep8 int(11) unsigned not null,
AtkStep9 int(11) unsigned not null
)ENGINE=InnoDB AUTO_INCREMENT=1 CHARSET=utf8;

create table if not exists StatTowerEquip (
ID int auto_increment primary key,
Dat datetime not null,
TowerID  varchar(128) not null,
EquipType  int(11) unsigned not null,
Step0Count int(11) unsigned not null,
Step1Count int(11) unsigned not null,
Step2Count int(11) unsigned not null,
Step3Count int(11) unsigned not null,
Step4Count int(11) unsigned not null,
Step5Count int(11) unsigned not null
)ENGINE=InnoDB AUTO_INCREMENT=1 CHARSET=utf8;

create table if not exists StatMaterialOutput (
ID int auto_increment primary key,
Dat datetime not null,
Tool varchar(128) not null,
OutPutCount int(11) unsigned not null
)ENGINE=InnoDB AUTO_INCREMENT=1 CHARSET=utf8;

create table if not exists StatShop (
ID int auto_increment primary key,
Dat datetime not null,
CommodityID varchar(128) not null,
CommodityName blob,
SellCount int(11) unsigned not null
)ENGINE=InnoDB AUTO_INCREMENT=1 CHARSET=utf8;