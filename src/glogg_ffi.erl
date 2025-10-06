-module(glogg_ffi).
-export([capture_stacktrace/0]).

capture_stacktrace() ->
    Stacktrace = try throw(stacktrace_capture)
                 catch _:_:Stk -> Stk
                 end,
    Frames = lists:map(fun format_frame/1, Stacktrace),
    [list_to_binary(F) || F <- Frames].

format_frame({Module, Function, Arity, Location}) ->
    File = case proplists:get_value(file, Location) of
        undefined -> "unknown";
        F when is_list(F) -> F;
        F when is_binary(F) -> binary_to_list(F);
        F -> lists:flatten(io_lib:format("~p", [F]))
    end,
    Line = proplists:get_value(line, Location, 0),
    lists:flatten(io_lib:format("~p:~p/~p (~s:~p)",
                                [Module, Function, Arity, File, Line])).
