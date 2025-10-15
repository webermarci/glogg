-module(logger_ffi).

-export([log/3, capture_stacktrace/0]).

log(LevelString, Message, Metadata) ->
    LevelAtom = level_ffi:level_binary_to_atom(LevelString),
    CleanMeta = deep_unwrap_dynamic(Metadata),
    logger:log(LevelAtom, Message, CleanMeta).

deep_unwrap_dynamic(Map) when is_map(Map) ->
    maps:map(fun(_K, V) -> deep_unwrap_dynamic(V) end, Map);
deep_unwrap_dynamic({gleam_dynamic, _Type, Value}) ->
    deep_unwrap_dynamic(Value);
deep_unwrap_dynamic(List) when is_list(List) ->
    [deep_unwrap_dynamic(E) || E <- List];
deep_unwrap_dynamic(Other) ->
    Other.

capture_stacktrace() ->
    Stacktrace =
        try
            throw(stacktrace_capture)
        catch
            _:_:Stk ->
                Stk
        end,
    Frames = lists:map(fun format_frame/1, Stacktrace),
    [list_to_binary(F) || F <- Frames].

format_frame({Module, Function, Arity, Location}) ->
    File =
        case proplists:get_value(file, Location) of
            undefined ->
                "unknown";
            F when is_list(F) ->
                F;
            F when is_binary(F) ->
                binary_to_list(F);
            F ->
                lists:flatten(
                    io_lib:format("~p", [F]))
        end,
    Line = proplists:get_value(line, Location, 0),
    lists:flatten(
        io_lib:format("~p:~p/~p (~s:~p)", [Module, Function, Arity, File, Line])).
