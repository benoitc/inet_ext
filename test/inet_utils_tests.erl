%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2018 6:02 PM
%%%-------------------------------------------------------------------
-module(inet_utils_tests).
-author("vdasari").

-include_lib("eunit/include/eunit.hrl").

simple_test_() ->
    [
        mac_to_tuple(),
        mac_to_string(),
        mac_to_binstring(),
        mac_to_list(),
        mac_to_binary(),
        mac_to_integer(),
        ip_to_tuple(),
        ip_to_string(),
        ip_to_binstring(),
        ip_to_list(),
        ip_to_binary(),
        ip_to_integer(),
        is_private_address(),
        is_global_address(),
        is_unspecified_address(),
        is_loopback_address(),
        is_linklocal_address(),
        is_multicast_address()
].


mac_addr(tuple) -> {1,2,3,4,5,6};
mac_addr(string) -> "01:02:03:04:05:06";
mac_addr(binstring) -> <<"01:02:03:04:05:06">>;
mac_addr(list) -> [1,2,3,4,5,6];
mac_addr(binary) -> <<1,2,3,4,5,6>>;
mac_addr(integer) -> 16#010203040506.

-define(mac_assert(Exp, To, From),
    ?_assertEqual(mac_addr(Exp),
        inet_utils:convert_mac(To, mac_addr(From))
    )
).
mac_to_tuple() -> [
    ?mac_assert(tuple, to_tuple, tuple),
    ?mac_assert(tuple, to_tuple, string),
    ?mac_assert(tuple, to_tuple, binstring),
    ?mac_assert(tuple, to_tuple, list),
    ?mac_assert(tuple, to_tuple, binary),
    ?mac_assert(tuple, to_tuple, integer)
].

mac_to_string() -> [
    ?mac_assert(string, to_string, tuple),
    ?mac_assert(string, to_string, string),
    ?mac_assert(string, to_string, binstring),
    ?mac_assert(string, to_string, list),
    ?mac_assert(string, to_string, binary),
    ?mac_assert(string, to_string, integer)
].

mac_to_binstring() -> [
    ?mac_assert(binstring, to_binstring, tuple),
    ?mac_assert(binstring, to_binstring, string),
    ?mac_assert(binstring, to_binstring, binstring),
    ?mac_assert(binstring, to_binstring, list),
    ?mac_assert(binstring, to_binstring, binary),
    ?mac_assert(binstring, to_binstring, integer)
].

mac_to_list() -> [
    ?mac_assert(list, to_list, tuple),
    ?mac_assert(list, to_list, string),
    ?mac_assert(list, to_list, binstring),
    ?mac_assert(list, to_list, list),
    ?mac_assert(list, to_list, binary),
    ?mac_assert(list, to_list, integer)
].

mac_to_binary() -> [
    ?mac_assert(binary, to_binary, tuple),
    ?mac_assert(binary, to_binary, string),
    ?mac_assert(binary, to_binary, binstring),
    ?mac_assert(binary, to_binary, list),
    ?mac_assert(binary, to_binary, binary),
    ?mac_assert(binary, to_binary, integer)
].

mac_to_integer() -> [
    ?mac_assert(integer, to_integer, tuple),
    ?mac_assert(integer, to_integer, string),
    ?mac_assert(integer, to_integer, binstring),
    ?mac_assert(integer, to_integer, list),
    ?mac_assert(integer, to_integer, binary),
    ?mac_assert(integer, to_integer, integer)
].

ip_addr(tuple) -> {1,2,3,4};
ip_addr(string) -> "1.2.3.4";
ip_addr(binstring) -> <<"1.2.3.4">>;
ip_addr(list) -> [1,2,3,4];
ip_addr(binary) -> <<1,2,3,4>>;
ip_addr(integer) -> 16#01020304.

-define(ip_assert(Exp, To, From),
    ?_assertEqual(ip_addr(Exp),
        inet_utils:convert_ip(To, ip_addr(From))
    )
).

ip_to_tuple() -> [
    ?ip_assert(tuple, to_tuple, tuple),
    ?ip_assert(tuple, to_tuple, string),
    ?ip_assert(tuple, to_tuple, binstring),
    ?ip_assert(tuple, to_tuple, list),
    ?ip_assert(tuple, to_tuple, binary),
    ?ip_assert(tuple, to_tuple, integer)
].

ip_to_string() -> [
    ?ip_assert(string, to_string, tuple),
    ?ip_assert(string, to_string, string),
    ?ip_assert(string, to_string, binstring),
    ?ip_assert(string, to_string, list),
    ?ip_assert(string, to_string, binary),
    ?ip_assert(string, to_string, integer)
].

ip_to_binstring() -> [
    ?ip_assert(binstring, to_binstring, tuple),
    ?ip_assert(binstring, to_binstring, string),
    ?ip_assert(binstring, to_binstring, binstring),
    ?ip_assert(binstring, to_binstring, list),
    ?ip_assert(binstring, to_binstring, binary),
    ?ip_assert(binstring, to_binstring, integer)
].

ip_to_list() -> [
    ?ip_assert(list, to_list, tuple),
    ?ip_assert(list, to_list, string),
    ?ip_assert(list, to_list, binstring),
    ?ip_assert(list, to_list, list),
    ?ip_assert(list, to_list, binary),
    ?ip_assert(list, to_list, integer)
].

ip_to_binary() -> [
    ?ip_assert(binary, to_binary, tuple),
    ?ip_assert(binary, to_binary, string),
    ?ip_assert(binary, to_binary, binstring),
    ?ip_assert(binary, to_binary, list),
    ?ip_assert(binary, to_binary, binary),
    ?ip_assert(binary, to_binary, integer)
].

ip_to_integer() -> [
    ?ip_assert(integer, to_integer, tuple),
    ?ip_assert(integer, to_integer, string),
    ?ip_assert(integer, to_integer, binstring),
    ?ip_assert(integer, to_integer, list),
    ?ip_assert(integer, to_integer, binary),
    ?ip_assert(integer, to_integer, integer)
].


is_private_address() ->
  [
   ?_assert(inet_ext:is_private_address("127.0.0.1")),
   ?_assert(inet_ext:is_private_address("192.168.0.1")),
   ?_assert(inet_ext:is_private_address("192.168.10.1")),
   ?_assert(inet_ext:is_private_address("172.16.10.1")),
   ?_assert(inet_ext:is_private_address("10.0.0.1")),
   ?_assert(inet_ext:is_private_address("10.100.0.1")),
   ?_assert(inet_ext:is_private_address("fe80::18f4:ac93:2ad0:6e01%en0"))
  ].

is_global_address() ->
  [
   ?_assert(inet_ext:is_global_address("217.182.142.173")),
   ?_assert(inet_ext:is_global_address("2a01:cb0c:8221:ed01:e810:5b2c:6800:ac5")),
   ?_assert(inet_ext:is_global_address("2a01:cb0c:8221:ed01:1053:5652:5b70:dea7")),
   ?_assertNot(inet_ext:is_global_address("192.168.0.1")),
   ?_assertNot(inet_ext:is_global_address("fe80::18f4:ac93:2ad0:6e01%en0"))
  ].

is_unspecified_address() ->
  [
   ?_assert(inet_ext:is_unspecified_address("0.0.0.0")),
   ?_assert(inet_ext:is_unspecified_address("::")),
   ?_assert(inet_ext:is_unspecified_address({0,0,0,0})),
   ?_assert(inet_ext:is_unspecified_address({0,0,0,0,0,0,0,0})),
   ?_assertNot(inet_ext:is_unspecified_address("10.0.0.1"))
  ].

is_loopback_address() ->
  [
   ?_assert(inet_ext:is_loopback_address("127.0.0.1")),
   ?_assert(inet_ext:is_loopback_address("::1")),
   ?_assertNot(inet_ext:is_loopback_address("10.0.0.1")),
   ?_assertNot(inet_ext:is_loopback_address("0.0.0.0")),
   ?_assertNot(inet_ext:is_loopback_address("::"))
  ].

is_linklocal_address() ->
  [
   ?_assert(inet_ext:is_linklocal_address("169.254.0.1")),
   ?_assert(inet_ext:is_linklocal_address("FE80:0000:0000:0000:C800:0EFF:FE74:0008")),
   ?_assertNot(inet_ext:is_linklocal_address("127.0.0.1")),
   ?_assertNot(inet_ext:is_linklocal_address("217.182.142.173")),
   ?_assertNot(inet_ext:is_linklocal_address("192.168.1.10")),
   ?_assertNot(inet_ext:is_linklocal_address("::1")),
   ?_assertNot(inet_ext:is_linklocal_address("2a01:cb0c:8221:ed01:e810:5b2c:6800:ac5"))
  ].

is_multicast_address() ->
  [
   ?_assert(inet_ext:is_multicast_address("224.0.0.1")),
   ?_assert(inet_ext:is_multicast_address("FF02:0:0:0:0:0:0:F")),
   ?_assertNot(inet_ext:is_multicast_address("192.168.1.1"))
  ].

