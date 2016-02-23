-module(inet_ext_lib).

-export([to_list/1]).
-export([run/1]).

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(S) when is_list(S) -> S.


run(Command) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    run_loop(Port, []).

run_loop(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            run_loop(Port, [Sofar|Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    true
            end,
            receive
                {'EXIT',  Port,  _} ->
                    ok
            after 1 ->              % force context switch
                      ok
            end,
            ExitCode =
            receive
                {Port, {exit_status, Code}} ->
                    Code
            end,
            {ExitCode, lists:flatten(Sofar)}
    end.
