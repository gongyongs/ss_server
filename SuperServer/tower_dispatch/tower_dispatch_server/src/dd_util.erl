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

%% API
-export([ensure_app_started/1]).
-export([to_list/1,
  to_integer/1,
  to_binary/1,
  timestamp/0,
  ipv4_to_str/1,
  md5_string/1,
  to_big/1,
  to_small/1,
  time_format/0,
  time_format/1,
  time_format_without_hms/1,
  milliseconds/0,
  encode_json_utf8/1
]).

-spec ensure_app_started(App :: atom()) -> ok.
ensure_app_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

encode_json_utf8(Any) ->
  JsonEncodeF = mochijson2:encoder([{utf8, true}]),
  JsonEncodeF(Any).

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
to_list(A) when is_integer(A) ->
  integer_to_list(A);
to_list(A) when is_list(A) -> A.


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

milliseconds() ->
  {MegaSecs, Secs, MicroSecs} = os:timestamp(),
  1000000000 * MegaSecs + Secs * 1000 + MicroSecs div 1000.