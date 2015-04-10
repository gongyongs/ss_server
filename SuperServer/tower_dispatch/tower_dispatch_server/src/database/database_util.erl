%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 九月 2014 上午11:20
%%%-------------------------------------------------------------------
-module(database_util).
-author("zqlt").

-include("../login/login.hrl").

%% API
-export([
  init_user_info/3,
  decode_user_info_rd/1,
  get_create_user_info_sql/1,
  update_user_info_sql/1
]).

-export([
  get_json_value_without_exception/3
]).

init_user_info(Device, PlayerID, Uin) when is_list(Device) andalso is_list(PlayerID) andalso is_integer(Uin) ->
 CurTime = dd_util:timestamp(),
  DisName = "guest" ++ dd_util:to_list(Uin + 101010),
 #user_info{
  uname = PlayerID,
  dis_name = DisName,
  uin = Uin,
  device = Device,
  platform_info = #platform{plat_type = "", player_id = PlayerID, player_dis_name = DisName, plat_friend = []},
  addition = #addition{},
  create_ts = CurTime
 }.

get_create_user_info_sql(UserInfo) when is_record(UserInfo, user_info) ->
  mysql_util:insert_query(
    "login",
    ["device", "uin", "uname", "dis_name", "platform_info", "addition", "create_ts"],
    [
      encode_device(UserInfo#user_info.device),
      encode_uin(UserInfo#user_info.uin),
      encode_player_id(UserInfo#user_info.uname),
      encode_dis_name(UserInfo#user_info.dis_name),
      encode_platform(UserInfo#user_info.platform_info),
      encode_addition(UserInfo#user_info.addition),
      encode_create_ts(UserInfo#user_info.create_ts)
    ]
  ).

update_user_info_sql(UserInfo) when is_record(UserInfo, user_info) ->
  mysql_util:update_query("login",
    ["device", "uname", "dis_name", "platform_info", "addition", "create_ts"],
    [
      encode_device(UserInfo#user_info.device),
      encode_player_id(UserInfo#user_info.uname),
      encode_dis_name(UserInfo#user_info.dis_name),
      encode_platform(UserInfo#user_info.platform_info),
      encode_addition(UserInfo#user_info.addition),
      encode_create_ts(UserInfo#user_info.create_ts)
    ], "uin=" ++ dd_util:to_list(UserInfo#user_info.uin) ++ ";").

decode_user_info_rd([Device,Uin, UName, DisName, Platform, Addition, CreateTs]) ->
  #user_info{
    device = decode_device(Device),
    uin = decode_uin(Uin),
    uname = decode_player_id(UName),
    dis_name = decode_dis_name(DisName),
    platform_info = decode_platform(Platform),
    addition = decode_addition(Addition),
    create_ts = decode_create_ts(CreateTs)
  }.


decode_player_id(PlayerID) -> dd_util:to_list(PlayerID).
decode_dis_name(DisName) -> dd_util:to_list(DisName).
decode_uin(Uin) -> dd_util:to_integer(Uin).
decode_device(Device) -> dd_util:to_list(Device).
decode_create_ts(CreateTs) -> dd_util:to_integer(CreateTs).

decode_platform(Platform) ->
  {struct, PlatformJsonList} = mochijson2:decode(Platform),
  Plat = dd_util:to_list(get_json_value(<<"plat">>, PlatformJsonList)),
  UName = dd_util:to_list(get_json_value(<<"id">>, PlatformJsonList)),
  DisName = dd_util:to_list(get_json_value(<<"name">>, PlatformJsonList)),
  FriendJsonList = get_json_value(<<"friend">>, PlatformJsonList),
  FriendList =
    lists:map(
      fun({struct, FriendItem}) ->
        ID = dd_util:to_list(get_json_value(<<"id">>, FriendItem)),
        Name = dd_util:to_list(get_json_value(<<"name">>, FriendItem)),
        #friend_item{id = ID, dis_name = Name}
      end, FriendJsonList),
  #platform{plat_type = Plat, player_id = UName, player_dis_name = DisName, plat_friend = FriendList}.

decode_addition(_Addition) ->
  #addition{}.


encode_player_id(PlayerID) when is_list(PlayerID) -> mysql_util:escape(PlayerID).
encode_dis_name(DisName) when is_list(DisName) -> mysql_util:escape(DisName).
encode_uin(Uin) when is_integer(Uin) -> dd_util:to_list(Uin).
encode_device(Device) when is_list(Device) -> mysql_util:escape(Device).


encode_addition(Addition) when is_record(Addition, addition) ->
  Json = {struct,[]},
  Value = mochijson2:encode(Json),
  mysql_util:escape(Value).

encode_platform(Platform) when is_record(Platform, platform) ->
  FriendList =
    lists:map(
      fun(FriendItem) ->
        {
          struct,
          [
            {<<"id">>, dd_util:to_binary(FriendItem#friend_item.id)},
            {<<"name">>, dd_util:to_binary(FriendItem#friend_item.dis_name)}
          ]
        }
      end, Platform#platform.plat_friend),
    Json =
    {
      struct,
      [
        {<<"id">>, dd_util:to_binary(Platform#platform.player_id)},
        {<<"plat">>, dd_util:to_binary(Platform#platform.plat_type)},
        {<<"name">>, dd_util:to_binary(Platform#platform.player_dis_name)},
        {<<"friend">>, FriendList}
      ]
    },
  Value = mochijson2:encode(Json),
  mysql_util:escape(Value).

encode_create_ts(CreateTs) when is_integer(CreateTs) -> dd_util:to_list(CreateTs).


get_json_value(Key, PropList) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined ->
      throw({custom, "error json key" ++ dd_util:to_list(Key)});
    Value -> Value
  end.

get_json_value_without_exception(Key, PropList, DefaultValue) ->
  case proplists:get_value(Key, PropList, undefined) of
    undefined -> DefaultValue;
    Value -> Value
  end.