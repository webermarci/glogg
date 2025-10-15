-module(handler_ffi).

-export([set_default_handler_json_formatting/0, set_default_handler_minimum_level/1,
         set_primary_minimum_level/1, add_handler/2, remove_handler/1]).

set_default_handler_json_formatting() ->
    logger:set_handler_config(default, formatter, {logger_formatter_json, #{}}).

set_default_handler_minimum_level(LevelString) ->
    LevelAtom = level_ffi:level_binary_to_atom(LevelString),
    logger:set_handler_config(default, level, LevelAtom).

set_primary_minimum_level(LevelString) ->
    LevelAtom = level_ffi:level_binary_to_atom(LevelString),
    logger:set_primary_config(level, LevelAtom).

add_handler(HandlerId, LevelString) ->
    HandlerIdAtom = binary_to_atom(HandlerId, utf8),
    LevelAtom = level_ffi:level_binary_to_atom(LevelString),
    logger:add_handler(HandlerIdAtom,
                       logger_std_h,
                       #{level => LevelAtom, formatter => {logger_formatter_json, #{}}}).

set_handler_minimum_level(HandlerId, LevelString) ->
    HandlerIdAtom = binary_to_existing_atom(HandlerId, utf8),
    LevelAtom = level_ffi:level_binary_to_atom(LevelString),
    logger:set_handler_config(HandlerIdAtom, level, LevelAtom).

remove_handler(HandlerId) ->
    HandlerIdAtom = binary_to_atom(HandlerId, utf8),
    logger:remove_handler(HandlerIdAtom).
