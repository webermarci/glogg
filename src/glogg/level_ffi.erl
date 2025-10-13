-module(level_ffi).

-export([level_binary_to_atom/1]).

level_binary_to_atom(<<"debug">>) ->
    debug;
level_binary_to_atom(<<"info">>) ->
    info;
level_binary_to_atom(<<"notice">>) ->
    notice;
level_binary_to_atom(<<"warning">>) ->
    warning;
level_binary_to_atom(<<"error">>) ->
    error;
level_binary_to_atom(<<"critical">>) ->
    critical;
level_binary_to_atom(<<"alert">>) ->
    alert;
level_binary_to_atom(<<"emergency">>) ->
    emergency.
