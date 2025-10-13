-module(logger_ffi).

-export([log/3, capture_stacktrace/0]).

deep_unwrap_and_atomize_keys(Map) when is_map(Map) ->
    maps:from_list([{maybe_atomize_key(K), deep_unwrap(V)} || {K, V} <- maps:to_list(Map)]);
deep_unwrap_and_atomize_keys(Other) ->
    deep_unwrap(Other).

maybe_atomize_key(K) when is_binary(K) ->
    try
        binary_to_existing_atom(K, utf8)
    catch
        error:badarg ->
            K
    end;
maybe_atomize_key(K) when is_atom(K) ->
    K;
maybe_atomize_key(K) ->
    K.

deep_unwrap(Map) when is_map(Map) ->
    maps:from_list([{maybe_atomize_key(K), deep_unwrap(V)} || {K, V} <- maps:to_list(Map)]);
deep_unwrap({Tag, Value}) when is_atom(Tag) ->
    deep_unwrap(Value);
deep_unwrap(List) when is_list(List) ->
    [deep_unwrap(E) || E <- List];
deep_unwrap(Other) ->
    Other.

log(LevelString, Message, Metadata) ->
    LevelAtom = level_ffi:level_binary_to_atom(LevelString),
    CleanMeta = deep_unwrap_and_atomize_keys(Metadata),
    logger:log(LevelAtom, Message, CleanMeta).

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
