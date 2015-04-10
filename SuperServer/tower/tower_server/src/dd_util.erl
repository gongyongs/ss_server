%%%-------------------------------------------------------------------
%%% @author zqlt
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 七月 2014 下午9:03
%%%-------------------------------------------------------------------
-module(dd_util).
-author("mickey").
-define(RAND_NUMBER, 25583).

%% API
-export([ensure_app_started/1]).
-export([to_list/1,
  to_integer/1,
  to_binary/1,
  float_to_integer/1,
  ipv4_to_str/1,
  statistics_list/1,
  statistics_list/2,
  random_in_range/1,
  random_in_range/2,
  get_ets_size/1,
  random/2,
  get_rand_val/1,
  md5_string/1,
  to_big/1,
  to_small/1,
  float_to_string/1,
  list_to_float/2,
  encode_json_utf8/1,
  filter_string/1
]).

-export([
  timestamp/0,
  time_format/0,
  time_format/1,
  time_format_without_hms/1,
  datetime_to_timestamp/1,
  timestamp_to_datetime/1,
  time_compare_by_timestamp/2,
  time_compare_by_datetime/2,
  to_local_time/1,
  to_universal_time/1,
  time_key/0,
  get_next_day/1,
  get_last_day/1,
  get_last_few_day/2,
  get_last_minute/1,
  get_last_few_minute/2,
  get_last_hour/1,
  get_last_few_hour/2,
  get_last_month/1,
  get_last_few_month/2,
  log_time_format/0,
  log_time_format/1,
  log_time_format_without_hms/0,
  log_time_format_without_hms/1,
  milliseconds/0
]).

-export([
  make_dir/2,
  map/2
]).
-spec ensure_app_started(App :: atom()) -> ok.
ensure_app_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

milliseconds() ->
  {MegaSecs, Secs, MicroSecs} = os:timestamp(),
  1000000000 * MegaSecs + Secs * 1000 + MicroSecs div 1000.

to_integer(undefined) -> 0;
to_integer("undefined") -> 0;
to_integer(<<"undefined">>) -> 0;
to_integer(I) when is_list(I) ->
  case catch list_to_integer(I) of
    {'EXIT', _} -> 0;
    Int -> Int
  end;
to_integer(I) when is_binary(I) ->
  list_to_integer(binary_to_list(I));
to_integer(I) when is_integer(I) ->
  I.

to_binary(A) when is_binary(A) ->
  A;
to_binary(A) when is_list(A) ->
  iolist_to_binary(A);
to_binary(A) when is_integer(A) ->
  list_to_binary(integer_to_list(A));
to_binary(A) when is_atom(A) ->
  atom_to_binary(A, utf8).
to_list(A) when is_binary(A) ->
  binary_to_list(A);
to_list(A) when is_atom(A) ->
  atom_to_list(A);
to_list(A) when is_integer(A) ->
  integer_to_list(A);
to_list(A) when is_list(A) -> A.


float_to_integer(V) when is_float(V) ->
  List = float_to_list(V),
  [Pre, [H | T]] = string:tokens(List, "e"),
  case H of
    $- ->  0;
    $+ ->
      Index = list_to_integer(T),
      if
        Index > 10 -> 0;
        Index =:= 0 ->
          [PreV, _SufV] = string:tokens(Pre, "."),
          list_to_integer(PreV);
        true ->
          [PreV, SufV] = string:tokens(Pre, "."),
          SS = lists:sublist(SufV, Index),
          list_to_integer(PreV ++ SS)
      end
  end;
float_to_integer(V) -> V.

check_dot('float', V) when is_list(V) ->
  case lists:member($., V) of
    false ->
      V ++ ".0";
    true ->
      V
  end.

list_to_float("undefined", DefaultV) -> DefaultV;
list_to_float(V, DefaultV) when is_list(V) andalso is_float(DefaultV) ->
  case V of
    [] ->
      DefaultV;
    _  ->
      NewV = check_dot('float', V),
      erlang:list_to_float(NewV)
  end;
list_to_float(V, _DefaultV) -> V.

%%后面两位小数
float_to_string(V) when is_float(V) ->
  List = float_to_list(V),
  [Pre, [H | T]] = string:tokens(List, "e"),
  case H of
    $- ->
      Index = list_to_integer(T),
      if
        Index =:= 0 -> lists:sublist(Pre, 4);
        Index =:= 1 ->
          [PreV, SufV] = string:tokens(Pre, "."),
          "0." ++ PreV ++ lists:sublist(SufV, 1);
        Index =:= 2 ->
          [PreV, SufV] = string:tokens(Pre, "."),
          "0.0" ++ PreV;
        true -> "0.00"
      end;
    $+ ->
      Index = list_to_integer(T),
      if
        Index > 10 -> 0;
        Index =:= 0 ->
          [PreV, SufV] = string:tokens(Pre, "."),
          PreV ++ "." ++lists:sublist(SufV, 2);
        true ->
          [PreV, SufV] = string:tokens(Pre, "."),
          SS = lists:sublist(SufV, Index),
          Dot = lists:sublist(SufV, Index + 1, 2),
          PreV ++ SS ++ "." ++ Dot
      end
  end;
float_to_string(V) -> V.

random_in_range(Max) ->
  random:seed(erlang:now()),
  trunc((random:uniform() * 1000000)) rem Max.

random_in_range(Min, Max) ->
  if
    Min =:= Max ->
      Min;
    true ->
      random:seed(erlang:now()),
      trunc((random:uniform() * 1000000)) rem (Max - Min + 1) + Min
  end.

%% 当前时间戳(秒数)
-spec timestamp() ->
  non_neg_integer().
timestamp() ->
  {MSecs, Secs, _} = os:timestamp(),
  MSecs * 1000000 + Secs.

-spec ipv4_to_str(Address :: inet:ip4_address()) ->
  string().
ipv4_to_str({A, B, C, D}) ->
  integer_to_list(A) ++ "." ++
    integer_to_list(B) ++ "." ++
    integer_to_list(C) ++ "." ++
    integer_to_list(D);
ipv4_to_str(StrIp4) when is_list(StrIp4) ->
  StrIp4;
ipv4_to_str(_) ->
  "noip".

md5_string(S) ->
  Md5_bin =  erlang:md5(S),
  Md5_list = binary_to_list(Md5_bin),
  lists:flatten(list_to_hex(Md5_list)).

to_big(L) -> [hex_to_big(H) || H <- L].
to_small(L) -> [hex_to_small(H) || H <- L].

hex_to_big(C) when C >= 97 andalso C =< 122 ->  C - 32;
hex_to_big(C) -> C.

hex_to_small(C) when C >= 65 andalso C =< 90 ->  C + 32;
hex_to_small(C) -> C.



%%
%% Local Functions
%%
list_to_hex(L) ->
  lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).

%%当前本地时间转换为时间戳
datetime_to_timestamp(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) -
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).

%%时间戳转化为本地时间
timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +
      calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).

%%时间本地化
to_local_time(DateTime) ->
  calendar:universal_time_to_local_time(DateTime).
to_universal_time(DateTime) ->
  [NDateTime] = calendar:local_time_to_universal_time_dst(DateTime),
  NDateTime.

%%timekey
time_key() ->
  {MSec, Sec, NSec} = erlang:now(),
  lists:flatten([dd_util:to_list(MSec), dd_util:to_list(Sec), dd_util:to_list(NSec)]).

%%时间格式化
time_format() ->
  {{Y, M, D},{H, MI, S}} = calendar:local_time(),
  time_format({{Y, M, D},{H, MI, S}}).
time_format({{Y, M, D},{H, MI, S}}) ->
  L =
    [
      integer_to_list(Y), "-",
      integer_to_list(M), "-",
      integer_to_list(D), " ",
      integer_to_list(H), ":",
      integer_to_list(MI), ":",
      integer_to_list(S)],
  lists:flatten(L).

time_format_without_hms({Y, M, D}) ->
  L =
    [
      integer_to_list(Y), "-",
      integer_to_list(M), "-",
      integer_to_list(D)
    ],
  lists:flatten(L).

%% -1 : L < R
%% 0 : L = R
%% 1: L > R
time_compare_by_timestamp(LTimestamp, RTimestamp) when is_integer(LTimestamp) andalso is_integer(RTimestamp) ->
  L = timestamp_to_datetime(LTimestamp),
  R = timestamp_to_datetime(RTimestamp),
  time_compare_by_datetime(L, R).

time_compare_by_datetime({{LYear, LMonth, LDay},{LHour, LMinute, LSecond}}, {{RYear, RMonth, RDay},{RHour, RMinute, RSecond}}) ->
  if
    LYear < RYear -> -1;
    LYear > RYear -> 1;
    true ->
      if
        LMonth < RMonth -> -1;
        LMonth > RMonth -> 1;
        true ->
          if
            LDay < RDay -> -1;
            LDay > RDay -> 1;
            true ->
              if
                LHour < RHour -> -1;
                LHour > RHour -> 1;
                true ->
                  if
                    LMinute < RMinute -> -1;
                    LMinute > RMinute -> 1;
                    true ->
                      if
                        LSecond < RSecond -> -1;
                        LSecond > RSecond -> 1;
                        true -> 0
                      end
                  end
              end
          end
      end
  end.

get_next_day({Year, Month, Day}) when is_integer(Year) andalso is_integer(Month) andalso is_integer(Day) ->
  DayOfMonth = calendar:last_day_of_the_month(Year,Month),
  if
    Day =:= DayOfMonth ->
      if
        Month =:= 12 ->
          {Year + 1, 1, 1};
        true ->
          {Year, Month + 1, 1}
      end;
    true ->
      {Year, Month, Day + 1}
  end.

%%获取昨天是几年几月几号
get_last_day({Year, Month, Day}) when is_integer(Year) andalso is_integer(Month) andalso is_integer(Day) ->
 if
   Day =:= 1 -> %%1号
      if
        Month =:= 1 ->
          {Year - 1, 12, 31};   %%1月1号的前一天是12月31号
        true ->
          AllDayOfMonth = calendar:last_day_of_the_month(Year,Month - 1),
          {Year, Month - 1, AllDayOfMonth}
      end;
   true ->
     {Year, Month, Day - 1}
 end.

%%获取前几天是几月几号
get_last_few_day({Year, Month, Day}, FewDay) when is_integer(Year) andalso is_integer(Month) andalso is_integer(Day) andalso FewDay > 0->
  lists:foldr(
    fun(_Index, {TmpYear, TmpMonth, TmpDay}) ->
      get_last_day({TmpYear, TmpMonth, TmpDay})
    end, {Year, Month, Day}, lists:seq(1, FewDay)).

get_last_month({Year, Month, Day}) ->
  if
    Month =:= 1 ->
      {Year - 1, 12, Day};
    true ->
      {Year, Month - 1, Day}
  end.

get_last_few_month({Year, Month, Day}, FewMonth) ->
  lists:foldr(
    fun(_Index, {TmpYear, TmpMonth, TmpDay}) ->
      get_last_month({TmpYear, TmpMonth, TmpDay})
    end, {Year, Month, Day}, lists:seq(1, FewMonth)).

get_last_hour({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  if
    Hour =:= 0 ->
      {get_last_day({Year, Month, Day}), {23, Minute, Second}};
    true ->
      {{Year, Month, Day}, {Hour - 1, Minute, Second}}
  end.

get_last_few_hour({{Year, Month, Day}, {Hour, Minute, Second}}, FewHour) ->
  lists:foldr(
    fun(_Index, {{TmpYear, TmpMonth, TmpDay},{TmpHour, TmpMinute, TmpSecond}}) ->
      get_last_hour({{TmpYear, TmpMonth, TmpDay}, {TmpHour, TmpMinute, TmpSecond}})
    end, {{Year, Month, Day}, {Hour, Minute, Second}}, lists:seq(1, FewHour)).

get_last_minute({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  if
    Minute =:= 0 ->
      get_last_hour({{Year, Month, Day}, {Hour, 59, Second}});
    true ->
      {{Year, Month, Day}, {Hour, Minute - 1, Second}}
  end.

get_last_few_minute({{Year, Month, Day}, {Hour, Minute, Second}}, FewMinute) ->
  lists:foldr(
    fun(_Index, {{TmpYear, TmpMonth, TmpDay},{TmpHour, TmpMinute, TmpSecond}}) ->
      get_last_minute({{TmpYear, TmpMonth, TmpDay}, {TmpHour, TmpMinute, TmpSecond}})
    end, {{Year, Month, Day}, {Hour, Minute, Second}}, lists:seq(1, FewMinute)).


to_2_col(V) when is_integer(V) ->
  to_2_col(dd_util:to_list(V));
to_2_col(V) when is_list(V) ->
  case length(V) of
    0 -> "00";
    1 -> "0" ++ V;
    2 -> V;
    _Other -> "00"
  end.

log_time_format_without_hms() ->
  {D, _T} = calendar:local_time(),
  log_time_format_without_hms(D).

log_time_format_without_hms({Year, Month, Day}) ->
  lists:flatten([dd_util:to_list(Year),to_2_col(Month),to_2_col(Day)]).

log_time_format() ->
  DT = calendar:local_time(),
  log_time_format(DT).

log_time_format({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  dd_util:to_list(Year) ++ to_2_col(Month) ++ to_2_col(Day) ++ "_" ++ to_2_col(Hour) ++ to_2_col(Minute) ++ to_2_col(Second).

%%[] -> [{id, count}]
statistics_list(List) when is_list(List) ->
  Tree =
    lists:foldr(
      fun(ID, TmpTree) ->
        AtomId = list_to_atom(dd_util:to_list(ID)),
        case gb_trees:lookup(AtomId, TmpTree) of
          none -> gb_trees:insert(AtomId, 1, TmpTree);
          {value, Count} -> gb_trees:update(AtomId, Count + 1, TmpTree)
        end
      end, gb_trees:empty(), List),

  lists:map(
    fun({ID, Count}) ->
      {dd_util:to_list(ID), Count}
    end, gb_trees:to_list(Tree)).

statistics_list(List, integer) when is_list(List) ->
  Tree =
    lists:foldr(
      fun(ID, TmpTree) ->
        AtomId = list_to_atom(dd_util:to_list(ID)),
        case gb_trees:lookup(AtomId, TmpTree) of
          none -> gb_trees:insert(AtomId, 1, TmpTree);
          {value, Count} -> gb_trees:update(AtomId, Count + 1, TmpTree)
        end
      end, gb_trees:empty(), List),

  lists:map(
    fun({ID, Count}) ->
      {dd_util:to_integer(dd_util:to_list(ID)), Count}
    end, gb_trees:to_list(Tree)).

filter_string(List) when is_list(List)->
  Set =
    lists:foldl(
      fun(Str, TmpSet) ->
        NStr = dd_util:to_list(Str),
        gb_sets:add_element(NStr, TmpSet)
      end, gb_sets:empty(), List),
  gb_sets:to_list(Set).

get_ets_size(TableID) ->
  InfoList = ets:info(TableID),
  get_ets_size_1(InfoList).
get_ets_size_1([]) -> 0;
get_ets_size_1([{size, Size} | T]) -> dd_util:to_integer(Size);
get_ets_size_1([{_Key, _Value} | T]) ->
  get_ets_size_1(T).

random(Seed, RandVal) when is_integer(Seed) andalso Seed > 0->
  Rand = get_rand_val(RandVal),
  Temp = (Rand div 65536) rem 32768,
  {(Temp*Seed) div 32768, Rand}.


get_rand_val(0) -> ?RAND_NUMBER;
get_rand_val(RandVal) when is_integer(RandVal) ->
  (RandVal * 200009 + 7057) band 16#FFFFFFFF.

make_dir(Path, DirName) when is_list(Path) andalso is_list(DirName) ->
  case file:list_dir(Path) of
    {ok, []} ->
      ND = Path ++ DirName ++ "/",
      case file:make_dir(ND) of
        ok -> {success, DirName};
        _Other -> {fail, "make dir error"}
      end;
    {ok, List} ->
      case lists:filter(fun(D) -> D =:= DirName end,List) of
        [] -> %%不存在
          ND = Path ++ DirName ++ "/",
          case file:make_dir(ND) of
            ok -> {success, DirName};
            _Other -> {fail, "make dir error"}
          end;
        [DirName] ->  {success, DirName};
        _Error -> {fail, "More Dir or Unknow error"}
      end;
    _Reason -> {fail, "ListDir Error"}
  end.

encode_json_utf8(Any) ->
  JsonEncodeF = mochijson2:encoder([{utf8, true}]),
  JsonEncodeF(Any).

map(F, List) ->
  map_1(List, F, []).
map_1([], _, OutList) -> lists:reverse(OutList);
map_1([H | T], F, OutList) -> map_1(T, F, [F(H) | OutList]).


