import glogg/level.{type Level, Debug, level_to_string}

@external(erlang, "handler_ffi", "set_default_handler_json_formatting")
@external(javascript, "./handler_ffi.mjs", "setDefaultHandlerJsonFormatting")
fn platform_set_default_handler_json_formatting() -> Nil

/// Sets the default handler to use JSON formatting.
pub fn set_default_handler_json_formatting() {
  platform_set_default_handler_json_formatting()
}

@external(erlang, "handler_ffi", "set_default_handler_minimum_level")
@external(javascript, "./handler_ffi.mjs", "setDefaultHandlerMinimumLevel")
fn platform_set_default_handler_minimum_level(level: String) -> Nil

/// Sets the minimum log level for the default handler.
pub fn set_default_handler_minimum_level(level: Level) {
  platform_set_default_handler_minimum_level(level_to_string(level))
}

@external(erlang, "handler_ffi", "set_primary_minimum_level")
@external(javascript, "./handler_ffi.mjs", "setPrimaryMinimumLevel")
fn platform_set_primary_minimum_level(level: String) -> Nil

/// Sets the global primary log level for the Erlang backend.
///
/// Messages below this level are discarded before reaching any handlers.
/// The default level is `Notice`. This is a no-op on the JavaScript target.
pub fn set_primary_minimum_level(level: Level) {
  platform_set_primary_minimum_level(level_to_string(level))
}

/// Sets up the default handler with JSON formatting and a minimum level of `Debug`.
/// Also sets the global primary minimum level to `Debug`.
/// This is a convenience function for quick setup.
pub fn setup_default_handler() {
  set_primary_minimum_level(Debug)
  set_default_handler_json_formatting()
  set_default_handler_minimum_level(Debug)
}

pub opaque type Handler {
  Handler(id: String, minimal_level: Level)
}

/// Creates a new handler with the given ID and a default minimum level of `Debug`.
/// The ID must be unique among all handlers.
///
/// # Example
///
/// ```gleam
/// let my_handler = handler.new("my_handler_id")
/// ```
pub fn new(id: String) -> Handler {
  Handler(id, Debug)
}

/// Creates a new handler with the given minimum level.
///
/// # Example
///
/// ```gleam
/// let my_handler = handler.new("my_handler_id")
///   |> handler.with_minimal_level(level.Info)
/// ```
pub fn with_minimal_level(handler: Handler, level: Level) -> Handler {
  Handler(handler.id, level)
}

/// Retrieves the ID of a handler.
pub fn get_id(handler: Handler) -> String {
  handler.id
}

@external(erlang, "handler_ffi", "add_handler")
@external(javascript, "./handler_ffi.mjs", "addHandler")
fn platform_add_handler(id: String, level: String) -> Nil

/// Adds a handler to the logging system.
///
/// On the Erlang target, the handler id is an atom,
/// which means creating too many handlers can exhaust the atom table.
/// That can lead to system instability.
/// It's recommended to create a limited number of handlers and reuse them.
///
/// # Example
///
/// ```gleam
/// let my_handler = handler.new("my_handler_id")
///   |> handler.with_minimal_level(level.Info)
///
/// handler.add(my_handler)
/// ```
pub fn add(handler: Handler) {
  platform_add_handler(handler.id, level_to_string(handler.minimal_level))
}

@external(erlang, "handler_ffi", "set_handler_minimum_level")
@external(javascript, "./handler_ffi.mjs", "setHandlerMinimumLevel")
fn platform_set_handler_minimum_level(id: String, level: String) -> Nil

/// Sets the minimum log level for a specific handler by its ID.
/// This can be used to change the log level of an existing handler.
///
/// # Example
///
/// ```gleam
/// handler.set_handler_minimum_level("my_handler_id", level.Warning)
/// // or
/// handler.set_handler_minimum_level(my_handler |> handler.get_id(), level.Warning)
/// ```
pub fn set_handler_minimum_level(id: String, level: Level) {
  platform_set_handler_minimum_level(id, level_to_string(level))
}

@external(erlang, "handler_ffi", "remove_handler")
@external(javascript, "./handler_ffi.mjs", "removeHandler")
fn platform_remove_handler(id: String) -> Nil

/// Removes a handler from the logging system by its ID.
///
/// # Example
///
/// ```gleam
/// handler.remove("my_handler_id")
/// // or
/// handler.remove(my_handler |> handler.get_id())
/// ```
pub fn remove(id: String) {
  platform_remove_handler(id)
}
