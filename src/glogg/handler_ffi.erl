-module(handler_ffi).

-export([configure_default_handler_json_formatting/0,
         configure_default_handler_minimum_level/1, add_handler/2, remove_handler/1]).

configure_default_handler_json_formatting() ->
    logger:set_handler_config(default, formatter, {logger_formatter_json, #{}}).

configure_default_handler_minimum_level(LevelString) ->
    LevelAtom = level_ffi:level_binary_to_atom(LevelString),
    logger:set_handler_config(default, level, LevelAtom).

add_handler(HandlerId, LevelString) ->
    LevelAtom = level_ffi:level_binary_to_atom(LevelString),
    logger:add_handler(HandlerId,
                       logger_std_h,
                       #{level => LevelAtom, formatter => {logger_formatter_json, #{}}}).

remove_handler(HandlerId) ->
    logger:remove_handler(HandlerId).
