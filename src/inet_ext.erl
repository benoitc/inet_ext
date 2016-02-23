-module(inet_ext).

-export([get_internal_address/1]).
-export([gateway_for/1]).
-export([gateways/0]).
-export([parse_address/1]).
-export([route/1, route/2]).
-export([routes/0]).



%% @doc get internal address used for this gateway
get_internal_address(Gateway) ->
	[{_, {MyIp, _}}|_] = route(parse_address(Gateway)),
	inet_parse:ntoa(MyIp).


gateway_for(IName0) ->
    IName = inet_ext_lib:to_list(IName0),
	case os:type() of
		{unix, linux} ->
			gateway_for1(IName, linux);
		{unix, darwin} ->
			gateway_for1(IName, darwin);
		{_, _} ->
            unsupported_platform
	end.


gateways() ->
    {ok, IFData} = inet:getifaddrs(),
    Interfaces = [I || {I, _} <- IFData],
    Gateways = lists:foldl(fun(IName, Acc) ->
                                   case gateway_for(IName) of
                                       undefined -> Acc;
                                       "" -> Acc;
                                       Ip -> [{IName,Ip} | Acc]
                                   end
                           end, [], Interfaces),
    lists:usort(Gateways).



gateway_for1(IName, linux) ->
    Cmd = "ip r | grep " ++ IName ++ " | grep default | cut -d ' ' -f 3",
    parse_result(inet_ext_lib:run(Cmd));
gateway_for1(IName, darwin) ->
    Cmd = "ipconfig getoption " ++ IName ++ " router",
    parse_result(inet_ext_lib:run(Cmd)).

parse_result({0, S0}) ->
    %% remove trailing endline
    case re:split(S0, "\n", [{return, list}, {parts, 2}]) of
        [S0] -> S0;
        [S, _] -> S
    end;
parse_result(_) ->
    undefined.



parse_address({_, _, _, _}=Addr) -> Addr;
parse_address({_, _, _, _, _, _, _, _}= Addr) -> Addr;
parse_address(S) ->
    {ok, Addr} = inet:parse_address(S),
    Addr.

%% convenient function to recover the list of routes
%% https://gist.github.com/archaelus/1247174
%% from @archaleus (Geoff Cant)
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
