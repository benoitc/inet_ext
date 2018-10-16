%% Copyright (c) 2009-2015, Vasu Dasari <vdasari@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(inet_utils).
-author("vdasari").

%% API exports
-export([convert_mac/2, convert_ip/2]).

-type convert_type() :: to_string | to_binstring | to_integer | to_binary | to_list | to_tuple.
-type ip() :: tuple() | list() | non_neg_integer() | binary().
-type mac_addr() :: binary() | list() | tuple() | non_neg_integer().


-export_types([convert_type/0, ip/0, mac_addr/0]).

%%====================================================================
%% API functions
%%====================================================================
-define(hexstr2int(H), erlang:list_to_integer(erlang:binary_to_list(H),16)).


-spec convert_mac(convert_type(), mac_addr()) -> term().
convert_mac(to_binstring, Arg) ->
  case convert_mac(to_string, Arg) of
    R when is_list(R) ->
      list_to_binary(R);
    R ->
      R
  end;
convert_mac(to_string, Arg) ->
  convert(to_string, convert_mac(to_binary, Arg));
convert_mac(to_integer, Arg) ->
  convert(to_integer, convert_mac(to_binary, Arg));
convert_mac(to_tuple, Arg) ->
  convert(to_tuple, convert_mac(to_binary, Arg));
convert_mac(to_list, Arg) ->
  convert(to_list, convert_mac(to_binary, Arg));

convert_mac(to_binary, Arg) when is_tuple(Arg), size(Arg) == 6 ->
  convert_mac(to_binary, erlang:list_to_binary(erlang:tuple_to_list(Arg)));

convert_mac(to_binary, Arg) when is_list(Arg), length(Arg) == 6 ->
  convert_mac(to_binary, erlang:list_to_binary(Arg));

convert_mac(to_binary, Arg) when is_list(Arg) ->
  convert_mac(to_binary, erlang:list_to_binary([erlang:list_to_integer(T, 16) || T <- string:tokens(Arg, ":")]));
convert_mac(to_binary, Arg) when is_binary(Arg), size(Arg) == 6 ->
  Arg;
convert_mac(to_binary, Arg) when is_binary(Arg) ->
  convert_mac(to_binary, binary_to_list(Arg));
convert_mac(to_binary, Arg) when is_integer(Arg) ->
  convert_mac(to_binary, binary:encode_unsigned(Arg)).

-spec convert_ip(convert_type(), ip()) -> term().
convert_ip(to_binstring, Arg) ->
  case convert_ip(to_string, Arg) of
    R when is_list(R) ->
      list_to_binary(R);
    R ->
      R
  end;
convert_ip(to_string, Arg) ->
  convert(to_string, convert_ip(to_binary, Arg));
convert_ip(to_integer, Arg) ->
  convert(to_integer, convert_ip(to_binary, Arg));
convert_ip(to_tuple, Arg) ->
  convert(to_tuple, convert_ip(to_binary, Arg));
convert_ip(to_list, Arg) ->
  convert(to_list, convert_ip(to_binary, Arg));

convert_ip(to_binary, Arg) when is_tuple(Arg), size(Arg) == 4 ->
  convert_ip(to_binary, erlang:list_to_binary(erlang:tuple_to_list(Arg)));
convert_ip(to_binary, Arg) when is_list(Arg), length(Arg) == 4 ->
  convert_ip(to_binary, erlang:list_to_binary(Arg));
convert_ip(to_binary, Arg) when is_integer(Arg) ->
  convert_ip(to_binary, binary:encode_unsigned(Arg));
convert_ip(to_binary, Arg) when is_list(Arg) ->
  case inet_parse:address(Arg) of
    {ok,Tuple} ->
      convert_ip(to_binary, Tuple);
    _ ->
      erlang:error(bad_arg)
  end;
convert_ip(to_binary, Arg) when is_binary(Arg), size(Arg) == 4 ->
  Arg;
convert_ip(to_binary, Arg) when is_binary(Arg) ->
  convert_ip(to_binary, binary_to_list(Arg)).

%%====================================================================
%% Internal functions
%%====================================================================

convert(Type, Bin) when is_binary(Bin) ->
  case Type of
    to_integer ->   binary:decode_unsigned(Bin);
    to_tuple ->     list_to_tuple(binary_to_list(Bin));
    to_list ->      binary_to_list(Bin);
    to_string ->
      case Bin of
        <<A,B,C,D>> ->
          lists:flatten(io_lib:format("~p.~p.~p.~p", [A,B,C,D]));
        <<A,B,C,D,E,F>> ->
          lists:flatten(io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B", [A,B,C,D,E,F]))
      end
  end;
convert(_, Arg) ->
  Arg.
