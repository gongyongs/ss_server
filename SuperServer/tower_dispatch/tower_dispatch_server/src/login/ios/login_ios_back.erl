%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十二月 2014 下午4:20
%%%-------------------------------------------------------------------
-module(login_ios_back).
-author("zqlt").

-include("../login.hrl").
-include("../../../deps/file_log/include/file_log.hrl").

%% API
-export([execute/3]).

execute(TableID, login_by_device, Device) ->
  ?FILE_LOG_DEBUG("login_by_device [~p]", [Device]),
  DBNode = get_database_node(),
  %% MatchSpec =  [{#user_info{device = '$1', _='_'}, [{'=:=', '$1', Device}], ['$_']}],
  %%case ets:select(TableID, MatchSpec) of
  case ets:match_object(TableID, #user_info{device = Device}) of
    [] ->
      case rpc:call(DBNode, database_monitor, execute, [get_user_by_device, Device]) of
        not_exist -> %%账户不存在
          case rpc:call(DBNode, database_monitor, execute, [create_user_by_device, Device]) of
            {success, UserInfo} ->
              ets:insert(TableID, UserInfo),
              {success, UserInfo};
            Other ->
              ?FILE_LOG_DEBUG("create_user_by_device error=> device = ~p, reason = ~p", [Device, Other]),
              {fail, "HintSystemError"}
          end;
        {success, UserInfo} ->
          ets:insert(TableID, UserInfo),
          {success, UserInfo};
        Other ->
          ?FILE_LOG_DEBUG("error ~p", [Other]),
          {fail, "HintSystemError"}
      end;
    [UserInfo] ->
      ets:update_element(TableID, Device, {#user_info.add_ts, dd_util:timestamp()}),
      {success, UserInfo}
  end;


execute(TableID, login_by_name, {Plat, Device, PlayerID, DisName, Friend}) ->
  ?FILE_LOG_DEBUG("login_by_name[~p,~p, ~p, ~p, ~p]", [Plat, Device, PlayerID, DisName, Friend]),
  DBNode = get_database_node(),
  %% MatchUNameSpec =  [{#user_info{uname = '$1', _='_'}, [{'=:=', '$1', PlayerID}], ['$_']}],
%%  MatchDeviceSpec = [{#user_info{device = '$1', _='_'}, [{'=:=', '$1', Device}], ['$_']}],
  case ets:match_object(TableID, #user_info{uname = PlayerID}) of
    [] ->
      case rpc:call(DBNode, database_monitor, execute, [get_user_by_uname, {PlayerID, Device}]) of
        {fail, Reason} ->
          ?FILE_LOG_DEBUG("get_user_by_uname error, uname = ~p, reason = ~p", [PlayerID, Reason]),
          {fail, "HintSystemError"};
        not_exist ->
          case ets:match_object(TableID, #user_info{device = Device}) of
            [] ->
              case rpc:call(DBNode, database_monitor, execute, [get_user_by_device, Device]) of
                not_exist -> %%设备信息也不存在，需要创建账户并且绑定该设备
                  case rpc:call(DBNode, database_monitor, execute, [create_user_by_uname, PlayerID]) of
                    {success, UserInfo} ->
                      NUserInfo = UserInfo#user_info{uname = PlayerID, dis_name = DisName, device = Device, addition = #addition{},
                      platform_info = #platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID, plat_type = Plat}, add_ts = dd_util:timestamp()},
                      ets:insert(TableID, NUserInfo),
                      success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                      {success, NUserInfo};
                    CROther ->
                      ?FILE_LOG_DEBUG("create_user_by_uname error => uname = ~p, reason = ~p", [PlayerID, CROther]),
                      {fail, "HintSystemError"}
                  end;
                {success, UserInfo} ->
                  case UserInfo#user_info.uname of
                    "" -> %%未绑定设备， 则绑定该设备
                      NUserInfo = UserInfo#user_info{uname = PlayerID, dis_name = DisName, device = Device, addition = #addition{},
                      platform_info = #platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID, plat_type = Plat}, add_ts = dd_util:timestamp()},
                      ets:insert(TableID, NUserInfo),
                      success = rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                      {success, NUserInfo};
                    _ -> %%已绑定其他设备, 则创建新的账户，device设为空
                      case rpc:call(DBNode, database_monitor, execute, [create_user_by_uname, PlayerID]) of
                        {success, UserInfo} ->
                          NUserInfo = UserInfo#user_info{uname = PlayerID, dis_name = DisName, device = "", addition = #addition{},
                          platform_info = #platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID, plat_type = Plat}, add_ts = dd_util:timestamp()},
                          ets:insert(TableID, NUserInfo),
                          success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                          {success, NUserInfo};
                        CROther ->
                          ?FILE_LOG_DEBUG("create_user_by_uname error => uname = ~p, reason = ~p", [PlayerID, CROther]),
                          {fail, "HintSystemError"}
                      end
                  end;
                Other ->
                  ?FILE_LOG_DEBUG("get user by device error => device = ~p, reason = ~p", [Device, Other]),
                  {fail, "HintSystemError"}
              end;
            [UserInfo] ->
              case UserInfo#user_info.uname of
                "" -> %%未绑定设备, 绑定该设备
                  NUserInfo = UserInfo#user_info{uname = PlayerID, dis_name = DisName, device = Device, addition = #addition{},
                  platform_info = #platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID, plat_type = Plat}, add_ts = dd_util:timestamp()},
                  ets:insert(TableID, NUserInfo),
                  success = rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                  {success, NUserInfo};
                _ ->  %%已绑定其他账户，则创建新账户，device设为空
                  case rpc:call(DBNode, database_monitor, execute, [create_user_by_uname, PlayerID]) of
                    {success, UNameUserInfo} ->
                      NUserInfo = UNameUserInfo#user_info{uname = PlayerID, dis_name = DisName, device = "", addition = #addition{},
                      platform_info = #platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID, plat_type = Plat}, add_ts = dd_util:timestamp()},
                      ets:insert(TableID, NUserInfo),
                      success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                      {success, NUserInfo};
                    CROther ->
                      ?FILE_LOG_DEBUG("create_user_by_uname error => uname = ~p, reason = ~p", [PlayerID, CROther]),
                      {fail, "HintSystemError"}
                  end
              end
          end;
        {success, UserInfo} ->   %%账户已经存在，检查账户是否绑定过设备，需要检查当设备是否已经绑定，如果未绑定，则需要将该设备绑定！
          CurPlatType = UserInfo#user_info.platform_info#platform.plat_type,
          if
            CurPlatType =/= "" andalso Plat =/= "" andalso  CurPlatType =/= Plat->
              ?FILE_LOG_DEBUG("platform info error, curPlat = ~p, Plat = ~p", [CurPlatType, Plat]),
              throw({custom, "HintLoginFailed"});
            true -> ok
          end,
          %%检查账户是否绑定设备
          case UserInfo#user_info.device of
            "" -> %%未绑定设备
              %%检查当前设备是否已绑定
              case ets:match_object(TableID, #user_info{device = Device}) of
                [] ->
                  case rpc:call(DBNode, database_monitor, execute, [get_user_by_device, Device]) of
                    not_exist -> %%设备信息不存在，绑定该设备
                      NUserInfo = UserInfo#user_info{dis_name = DisName, device = Device, add_ts = dd_util:timestamp(),
                      platform_info = UserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID}},
                      ets:insert(TableID, NUserInfo),
                      success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                      {success, NUserInfo};
                    {success, _} -> %%设备信息已经存在，不能绑定
                      NUserInfo = UserInfo#user_info{dis_name = DisName, add_ts = dd_util:timestamp(),
                      platform_info = UserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID}},
                      ets:insert(TableID, NUserInfo),
                      success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                      {success, NUserInfo};
                    GUOther ->
                      ?FILE_LOG_DEBUG("get_user_by_device error => reason = ~p, device = ~p", [GUOther, Device]),
                      {fail, "HintSystemError"}
                  end;
                [_DevUserInfo] ->  %%存在设备信息，不能绑定
                  NUserInfo = UserInfo#user_info{dis_name = DisName, add_ts = dd_util:timestamp(),
                  platform_info = UserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID}},
                  ets:insert(TableID, NUserInfo),
                  success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                  {success, NUserInfo}
              end;
            _ -> %%已绑定设备
              NUserInfo = UserInfo#user_info{dis_name = DisName, add_ts = dd_util:timestamp(),
              platform_info = UserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID}},
              ets:insert(TableID, NUserInfo),
              success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
              {success, NUserInfo}
          end
      end;
    [UNameUserInfo] ->
      CurPlatType = UNameUserInfo#user_info.platform_info#platform.plat_type,
      if
        CurPlatType =/= "" andalso Plat =/= "" andalso  CurPlatType =/= Plat->
          ?FILE_LOG_DEBUG("platform info error, curPlat = ~p, Plat = ~p", [CurPlatType, Plat]),
          throw({custom, "HintLoginFailed"});
        true -> ok
      end,
      %%检查账户是否绑定设备
      case UNameUserInfo#user_info.device of
        "" -> %%未绑定设备
          %%检查当前设备是否已绑定
          case ets:match_object(TableID, #user_info{device = Device}) of
            [] ->
              case rpc:call(DBNode, database_monitor, execute, [get_user_by_device, Device]) of
                not_exist -> %%设备信息不存在，绑定该设备
                  NUserInfo = UNameUserInfo#user_info{dis_name = DisName, device = Device, add_ts = dd_util:timestamp(),
                  platform_info = UNameUserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID}},
                  ets:insert(TableID, NUserInfo),
                  success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                  {success, NUserInfo};
                {success, _} -> %%设备信息已经存在，不能绑定
                  NUserInfo = UNameUserInfo#user_info{dis_name = DisName, add_ts = dd_util:timestamp(),
                  platform_info = UNameUserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID}},
                  ets:insert(TableID, NUserInfo),
                  success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                  {success, NUserInfo};
                GUOther ->
                  ?FILE_LOG_DEBUG("get_user_by_device error => reason = ~p, device = ~p", [GUOther, Device]),
                  {fail, "HintSystemError"}
              end;
            [_DevUserInfo] ->  %%存在设备信息，不能绑定
              NUserInfo = UNameUserInfo#user_info{dis_name = DisName, add_ts = dd_util:timestamp(),
              platform_info = UNameUserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID}},
              ets:insert(TableID, NUserInfo),
              success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
              {success, NUserInfo}
          end;
        _ ->
          NUserInfo = UNameUserInfo#user_info{dis_name = DisName, add_ts = dd_util:timestamp(),
          platform_info = UNameUserInfo#user_info.platform_info#platform{player_dis_name = DisName, plat_friend = Friend#friends.friend_list, player_id = PlayerID}},
          ets:insert(TableID, NUserInfo),
          success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
          {success, NUserInfo}
      end
  end;

execute(TableID, check_device, Device) ->
  ?FILE_LOG_DEBUG("check_device [~p]", [Device]),
  DBNode = get_database_node(),
  %%MatchDeviceSpec = [{#user_info{device = '$1', _='_'}, [{'=:=', '$1', Device}], ['$_']}],
  case ets:match_object(TableID, #user_info{device = Device}) of
    [] ->
      case rpc:call(DBNode, database_monitor, execute, [check_device, Device]) of
        not_exist -> {success, 0};
        {success, UserInfo} ->
          case UserInfo#user_info.uname of
            "" -> {success, 0};
            _ -> {success, 1}
          end;
        Other ->
          ?FILE_LOG_DEBUG("check_device error, device = ~p, reason = ~p", [Device, Other]),
          {fail, "HintSystemError"}
      end;
    [UserInfo] ->
      case UserInfo#user_info.uname of
        "" -> {success, 0};
        _ -> {success, 1}
      end
  end;
%%绑定设备
execute(TableID, bond_device, {Plat, Device, PlayerID, DisName, Friend}) ->
  ?FILE_LOG_DEBUG("bonde_device [~p]", [Device]),
  DBNode = get_database_node(),
  %% MatchDeviceSpec = [{#user_info{device = '$1', _='_'}, [{'=:=', '$1', Device}], ['$_']}],
  %%MatchUNameSpec =  [{#user_info{uname = '$1', _='_'}, [{'=:=', '$1', PlayerID}], ['$_']}],
  case ets:match_object(TableID, #user_info{device = Device}) of
    [] ->
      case rpc:call(DBNode, database_monitor, execute, [get_user_by_device, Device]) of
        not_exist ->
          ?FILE_LOG_DEBUG("get_user_by_device not exist", []),
          {fail, "HintSystemError"};
        {success, UserInfo} ->  %%device 账户存在，检查uname账户是否存在
          case UserInfo#user_info.uname of
            "" -> %%未绑定
              case ets:match_object(TableID, #user_info{uname = PlayerID}) of
                [] ->
                  case rpc:call(DBNode, database_monitor, execute, [get_user_by_uname, {PlayerID, Device}]) of
                    not_exist -> %%UNAME账户不存在，则应该成功绑定该账户
                      NUserInfo = UserInfo#user_info{uname = PlayerID, dis_name = DisName, add_ts = dd_util:timestamp(),
                      platform_info = #platform{plat_friend = Friend#friends.friend_list, player_dis_name = DisName, plat_type = Plat, player_id = PlayerID}},
                      ets:insert(TableID, NUserInfo),
                      success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                      {success, NUserInfo};
                    {success, UNameUserInfo} ->
                      if
                        UNameUserInfo#user_info.uin =:= UserInfo#user_info.uin ->  %%绑定，更新好友数据
                          NUserInfo = UNameUserInfo#user_info{device = Device, dis_name = DisName, add_ts = dd_util:timestamp(),
                          platform_info = UNameUserInfo#user_info.platform_info#platform{plat_friend = Friend#friends.friend_list, player_dis_name = DisName}},
                          success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                          ets:insert(TableID, NUserInfo),
                          {success, NUserInfo};
                        true ->   %%账户不同,不能绑定
                          {fail, "HintIlegalBondAccount"}
                      end;
                    UNOther ->
                      ?FILE_LOG_DEBUG("create_user_by_uname error=> reason = ~p, uname = ~p", [UNOther, PlayerID]),
                      {fail, "HintSystemError"}
                  end;
                [UNameUserInfo] ->
                  if
                    UNameUserInfo#user_info.uin =:= UserInfo#user_info.uin ->  %%绑定，更新好友数据
                      NUserInfo = UNameUserInfo#user_info{device = Device, dis_name = DisName, add_ts = dd_util:timestamp(),
                      platform_info = UNameUserInfo#user_info.platform_info#platform{plat_friend = Friend#friends.friend_list, player_dis_name = DisName}},
                      success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                      ets:insert(TableID, NUserInfo),
                      {success, NUserInfo};
                    true ->   %%账户不同,不能绑定
                      {fail, "HintIlegalBondAccount"}
                  end
              end;
            _ -> %%已绑定
              account_has_bond
          end;
        GTOther ->
          ?FILE_LOG_DEBUG("get_user_by_device error => reason = ~p, device = ~p", [GTOther, Device]),
          {fail, "HintSystemError"}
      end;
    [UserInfo] ->
      case ets:match_object(TableID, #user_info{uname = PlayerID}) of
        [] ->
          case rpc:call(DBNode, database_monitor, execute, [get_user_by_uname, {PlayerID, Device}]) of
            not_exist -> %%UNAME账户不存在，则应该成功绑定该账户
              NUserInfo = UserInfo#user_info{uname = PlayerID, dis_name = DisName, add_ts = dd_util:timestamp(),
              platform_info = #platform{plat_friend = Friend#friends.friend_list, player_dis_name = DisName, plat_type = Plat, player_id = PlayerID}},
              ets:insert(TableID, NUserInfo),
              success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
              {success, NUserInfo};
            {success, UNameUserInfo} ->
              if
                UNameUserInfo#user_info.uin =:= UserInfo#user_info.uin ->  %%绑定，更新好友数据
                  NUserInfo = UNameUserInfo#user_info{device = Device, dis_name = DisName, add_ts = dd_util:timestamp(),
                  platform_info = UNameUserInfo#user_info.platform_info#platform{plat_friend = Friend#friends.friend_list, player_dis_name = DisName}},
                  success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
                  ets:insert(TableID, NUserInfo),
                  {success, NUserInfo};
                true ->   %%账户不同,不能绑定
                  {fail, "HintIlegalBondAccount"}
              end;
            UNOther ->
              ?FILE_LOG_DEBUG("create_user_by_uname error=> reason = ~p, uname = ~p", [UNOther, PlayerID]),
              {fail, "HintSystemError"}
          end;
        [UNameUserInfo] ->
          if
            UNameUserInfo#user_info.uin =:= UserInfo#user_info.uin ->  %%绑定，更新好友数据
              NUserInfo = UNameUserInfo#user_info{device = Device, dis_name = DisName, add_ts = dd_util:timestamp(),
              platform_info = UNameUserInfo#user_info.platform_info#platform{plat_friend = Friend#friends.friend_list, player_dis_name = DisName}},
              success =  rpc:call(DBNode, database_monitor, execute, [update_user_info, NUserInfo]),
              ets:insert(TableID, NUserInfo),
              {success, NUserInfo};
            true ->   %%账户不同,不能绑定
              {fail, "HintIlegalBondAccount"}
          end
      end
  end.


get_database_node() ->
  case ets:lookup(login_cfg, database_node) of
    [] ->
      ?FILE_LOG_DEBUG("database node not exist", []),
      throw({custom, "HintSystemError"});
    [#login_cfg{value = Node}] -> Node
  end.
