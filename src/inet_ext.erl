%%% -*- erlang -*-
%%% This file is part of nat-pmp released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016 Benoît Chesneau <benoitc@refuge.io>

-module(inet_ext).

-export([get_internal_address/1]).
-export([gateways/0]).
-export([gateway_for/1]).
-export([parse_address/1]).
-export([route/1, route/2]).
-export([routes/0]).
-export([is_private_address/1]).
-export([is_global_address/1]).
-export([is_loopback_address/1]).
-export([is_unspecified_address/1]).
-export([is_reserved_address/1]).
-export([is_linklocal_address/1]).
-export([is_multicast_address/1]).

-include("inet_ext.hrl").

%% @doc get internal address used for this gateway
-spec get_internal_address(Gateway) -> IP when
    Gateway :: inet:ip_address() | inet:hostname(),
    IP :: string().
get_internal_address(Gateway) ->
  [{_, {MyIp, _}}|_] = route(parse_address(Gateway)),
  inet_parse:ntoa(MyIp).



%% @doc return the gateway IPs for each platform
-spec gateways() -> [{Interface, IP}] when
    Interface :: atom(),
    IP :: string().
gateways() ->
  {ok, IFData} = inet:getifaddrs(),
  Interfaces = [I || {I, _} <- IFData],
  Gateways = lists:foldl(fun(IName, Acc) ->
                             Ip = gateway_for(IName),
                             [{IName,Ip} | Acc]
                         end, [], Interfaces),
  lists:usort(Gateways).

gateway_for(IName0) ->
  IName = inet_ext_lib:to_list(IName0),
  case os:type() of
    {unix, linux} ->
      gateway_for(IName, linux);
    {unix, darwin} ->
      gateway_for(IName, darwin);
    {unix, _} ->
      gateway_for(IName, bsd);
    {win32, _} ->
      gateway_for(IName, win32)
  end.

gateway_for(IName, linux) ->
  Cmd = "ip r | grep " ++ IName ++ " | grep default | sed 's/^none //' | cut -d ' ' -f 3",
  parse_result(inet_ext_lib:run(Cmd));
gateway_for(IName, darwin) ->
  Cmd = "ipconfig getoption " ++ IName ++ " router",
  parse_result(inet_ext_lib:run(Cmd));
gateway_for(IName, bsd) ->
  Cmd = "netstat -rn |grep " ++ IName ++ "|grep default|awk '{print $2}'",
  parse_result(inet_ext_lib:run(Cmd));
gateway_for(IName, win32) ->
  case re:split(IName, "_", [{return, list}, {parts, 2}]) of
    [_, SettingId] ->
      Cmd = "wmic nicconfig where 'SettingId=\"" ++ SettingId ++
      "\"' get DefaultIPGateway /format:csv",
      parse_win_result(inet_ext_lib:run(Cmd));
    _Else ->
      undefined
  end.

parse_result({0, S0}) ->
  %% remove trailing endline
  case re:split(S0, "\n", [{return, list}, {parts, 2}]) of
    [S0] -> S0;
    [S, _] -> S
  end;
parse_result(_) ->
  undefined.

parse_win_result({0, Res}) ->
  [_, _, Line| _] = re:split(Res, "\r\r\n", [{return, list}]),
  case re:split(Line, ",", [{return, binary}]) of
    [_, <<>>] ->
      undefined;
    [_, GatewayListBin] ->
      case binary:split(GatewayListBin, <<";">>) of
        [<< "{", IP/binary >>, _] ->
          binary_to_list(IP);
        [GatewayListBin] ->
          IP = binary:part(GatewayListBin,
                           1, byte_size(GatewayListBin) - 2),
          binary_to_list(IP)
      end;
    _ ->
      undefined
  end;
parse_win_result(_) ->
  undefined.

%% @doc convenient function to parse an address
-spec parse_address(AddrIn) -> AddrOut when
    AddrIn :: inet:ip_address() | inet:hostname(),
    AddrOut :: inet:ip_address().
parse_address({_, _, _, _}=Addr) -> Addr;
parse_address({_, _, _, _, _, _, _, _}= Addr) -> Addr;
parse_address(S) ->
  {ok, Addr} = inet:parse_address(S),
  Addr.

%% convenient function to recover the list of routes
%% https://gist.github.com/archaelus/1247174@@@
%% from @archaleus (Geoff Cant)
%%
%%
%% @doc get the route information for an IP address
-spec route(IP) -> [{Interface, {Route, NetMask}}] when
    IP :: inet:ip_address(),
    Interface :: atom(),
    Route :: inet:ip_address(),
    NetMask :: inet:ip_address().
route(Targ) ->
  route(Targ, routes()).

route(Targ, Routes) ->
  sort_routes(routes_for(Targ, Routes)).

routes_for(Targ, Routes) ->
  [ RT || RT = {_IF, {Addr, Mask}} <- Routes,
          tuple_size(Targ) =:= tuple_size(Addr),
          match_route(Targ, Addr, Mask)
  ].

sort_routes(Routes) ->
  lists:sort(fun ({_, {_AddrA, MaskA}}, {_, {_AddrB, MaskB}}) ->
                 MaskA > MaskB
             end,
             Routes).

match_route(Targ, Addr, Mask)
  when tuple_size(Targ) =:= tuple_size(Addr),
       tuple_size(Targ) =:= tuple_size(Mask) ->
  lists:all(fun (A) -> A end,
            [element(I, Targ) band element(I, Mask)
             =:= element(I, Addr) band element(I, Mask)
             || I <- lists:seq(1, tuple_size(Targ)) ]).


%% @doc get all routes
-spec routes() -> [{Interface, {Route, NetMask}}] when
    Interface :: atom(),
    Route :: inet:ip_address(),
    NetMask :: inet:ip_address().
routes() ->
  {ok, IFData} = inet:getifaddrs(),
  lists:append([ routes(IF, IFOpts) || {IF, IFOpts} <- IFData ]).

routes(IF, Opts) ->
  {_,Routes} = lists:foldl(fun parse_opts/2, {undefined, []}, Opts),
  [{IF, Route}  || Route <- Routes].

parse_opts({addr, Addr}, {undefined, Routes}) ->
  {{addr, Addr}, Routes};
parse_opts({netmask, Mask}, {{addr, Addr}, Routes})
  when tuple_size(Mask) =:= tuple_size(Addr) ->
  {undefined, [{Addr, Mask} | Routes]};
parse_opts(_, Acc) -> Acc.



is_private_address({_, _, _, _}=Addr)->
  is_network(?IPv4_PRIVATE_NETWORKS, Addr);
is_private_address({_, _, _, _, _, _, _, _}=Addr) ->
  is_network(?IPv6_PRIVATE_NETWORKS, Addr);
is_private_address(Addr) when is_list(Addr) ->
  is_private_address(parse_address(Addr));
is_private_address(_) ->
  false.


is_global_address(Addr) ->
  (is_private_address(Addr) =:= false).


is_loopback_address({_, _, _, _} = Addr) ->
  inet_cidr:contains(?IPv4_LOOPBACK_NETWORK, Addr);
is_loopback_address({0, 0, 0, 0, 0, 0, 0, 1}) ->
  true;
is_loopback_address(Addr) when is_list(Addr) ->
  is_loopback_address(parse_address(Addr));
is_loopback_address(_) ->
  false.

is_unspecified_address({0, 0, 0, 0}) -> true;
is_unspecified_address({0, 0, 0, 0, 0, 0, 0, 0}) -> true;
is_unspecified_address(Addr) when is_list(Addr) ->
  is_unspecified_address(parse_address(Addr));
is_unspecified_address(_) ->
  false.

is_reserved_address({_, _, _, _}=Addr) ->
  inet_cidr:contains(?IPv4_RESERVED_NETWORK, Addr);
is_reserved_address({_, _, _, _, _, _, _, _}=Addr) ->
  is_network(?IPv6_RESERVED_NETWORKS, Addr);
is_reserved_address(Addr) when is_list(Addr) ->
  is_reserved_address(parse_address(Addr));
is_reserved_address(_) ->
  false.


is_linklocal_address({_, _, _, _}=Addr) ->
  inet_cidr:contains(?IPv4_LINKLOCAL_NERWORK, Addr);
is_linklocal_address({_, _, _, _, _, _, _, _}=Addr) ->
  inet_cidr:contains(?IPv6_LINKLOCAL_NETWORK, Addr);
is_linklocal_address(Addr) when is_list(Addr) ->
  is_linklocal_address(parse_address(Addr));
is_linklocal_address(_) ->
  false.

is_multicast_address({_, _, _, _}=Addr) ->
  inet_cidr:contains(?IPv4_MULTICAST_NETWORK, Addr);
is_multicast_address({_, _, _, _, _, _, _, _}=Addr) ->
  inet_cidr:contains(?IPv6_MULTICAST_NETWORK, Addr);
is_multicast_address(Addr) when is_list(Addr) ->
  is_multicast_address(parse_address(Addr));
is_multicast_address(_) ->
  false.


%% check if an ip is a member of the test networks
is_network([Net | Rest], Addr) ->
  case inet_cidr:contains(Net, Addr) of
    true -> true;
    false -> is_network(Rest, Addr)
  end;
is_network([], _Addr) ->
  false.
