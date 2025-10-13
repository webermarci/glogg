import glogg/level.{type Level, Debug, level_to_string}

@external(erlang, "handler_ffi", "configure_default_handler_json_formatting")
@external(javascript, "./handler_ffi.mjs", "configureDefaultHandlerJsonFormatting")
fn platform_configure_default_handler_json_formatting() -> Nil

/// Configures the default handler to use JSON formatting.
pub fn configure_default_handler_json_formatting() {
  platform_configure_default_handler_json_formatting()
}

@external(erlang, "handler_ffi", "configure_default_handler_minimum_level")
@external(javascript, "./handler_ffi.mjs", "configureDefaultHandlerMinimumLevel")
fn platform_configure_default_handler_minimum_level(level: String) -> Nil

/// Configures the minimum log level for the default handler.
pub fn configure_default_handler_minimum_level(level: Level) {
  platform_configure_default_handler_minimum_level(level_to_string(level))
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
