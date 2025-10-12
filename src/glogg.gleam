import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/time/duration.{type Duration}

@external(erlang, "glogg_ffi", "configure_default_json_formatting")
@external(javascript, "./glogg_ffi.mjs", "configureDefaultJsonFormatting")
fn platform_configure_default_json_formatting() -> Nil

/// Configures all loggers to use JSON formatting by default.
pub fn configure_default_json_formatting() {
  platform_configure_default_json_formatting()
}

@external(erlang, "glogg_ffi", "configure_default_minimum_level")
@external(javascript, "./glogg_ffi.mjs", "configureDefaultMinimumLevel")
fn platform_configure_default_minimum_level(level: String) -> Nil

/// Configures all loggers to use the specified minimum log level by default.
/// Messages below this level will be ignored.
pub fn configure_default_minimum_level(level: Level) {
  platform_configure_default_minimum_level(level_to_string(level))
}

@external(erlang, "glogg_ffi", "log")
@external(javascript, "./glogg_ffi.mjs", "log")
fn platform_log(
  level: String,
  message: String,
  metadata: Dict(String, dynamic.Dynamic),
) -> Nil

@external(erlang, "glogg_ffi", "capture_stacktrace")
@external(javascript, "./glogg_ffi.mjs", "captureStacktrace")
fn platform_capture_stacktrace() -> List(Dynamic)

pub type Level {
  Debug
  Info
  Notice
  Warning
  Error
  Critical
  Alert
  Emergency
}

fn level_to_string(level: Level) -> String {
  case level {
    Debug -> "debug"
    Info -> "info"
    Notice -> "notice"
    Warning -> "warning"
    Error -> "error"
    Critical -> "critical"
    Alert -> "alert"
    Emergency -> "emergency"
  }
}

pub opaque type Field {
  String(key: String, value: String)
  Bool(key: String, value: Bool)
  Int(key: String, value: Int)
  Float(key: String, value: Float)
  Group(key: String, fields: List(Field))
  Stacktrace
}

pub opaque type Logger {
  Logger(name: String, context: List(Field))
}

/// Creates a new logger.
///
/// The logger will have a default context field "logger" with the given name.
///
/// # Example
///
/// ```gleam
/// let logger = glogg.new_logger("name")
/// ```
pub fn new_logger(name: String) -> Logger {
  Logger(name, [String("logger", name)])
}

/// Adds context fields to a logger.
///
/// These fields will be included in every log message.
///
/// # Example
///
/// ```gleam
/// let logger =
///   glogg.new_logger("name")
///   |> glogg.with_context([
///     glogg.string("app", "my_app"),
///     glogg.string("env", "production"),
///   ])
/// ```
pub fn with_context(logger: Logger, fields: List(Field)) -> Logger {
  Logger(logger.name, list.append(logger.context, fields))
}

/// Retrieves the context fields of a logger.
pub fn get_context(logger: Logger) -> List(Field) {
  logger.context
}

/// Logs a message and fields at the `Debug` level.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.debug("Message", [
///   glogg.string("key", "value"),
/// ])
/// ```
pub fn debug(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Debug, message, fields)
}

/// Logs a message and fields at the `Info` level.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.info("Message", [
///   glogg.string("key", "value"),
/// ])
/// ```
pub fn info(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Info, message, fields)
}

/// Logs a message and fields at the `Notice` level.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.notice("Message", [
///   glogg.string("key", "value"),
/// ])
/// ```
pub fn notice(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Notice, message, fields)
}

/// Logs a message and fields at the `Warning` level.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.warning("Message", [
///   glogg.string("key", "value"),
/// ])
/// ```
pub fn warning(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Warning, message, fields)
}

/// Logs a message and fields at the `Error` level to standard error.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.error("Message", [
///   glogg.string("key", "value"),
///   glogg.stacktrace(),
/// ])
/// ```
pub fn error(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Error, message, fields)
}

/// Logs a message and fields at the `Critical` level to standard error.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.critical("Message", [
///   glogg.string("key", "value"),
///   glogg.stacktrace(),
/// ])
/// ```
pub fn critical(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Critical, message, fields)
}

/// Logs a message and fields at the `Alert` level to standard error.
///
/// # Example
///
/// ```gleam
/// glogg.alert(logger, "Message", [
///   glogg.string("key", "value"),
///   glogg.stacktrace(),
/// ])
/// ```
pub fn alert(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Alert, message, fields)
}

/// Logs a message and fields at the `Emergency` level to standard error.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.emergency(logger, "Message", [
///   glogg.string("key", "value"),
///   glogg.stacktrace(),
/// ])
/// ```
pub fn emergency(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Emergency, message, fields)
}

/// Creates a string field with the given key and value.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.info("Message", [
///   glogg.string("key", "value"),
/// ])
/// ```
pub fn string(key: String, value: String) -> Field {
  String(key:, value:)
}

/// Creates a boolean field with the given key and value.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.info("Message", [
///   glogg.bool("key", True),
/// ])
/// ```
pub fn bool(key: String, value: Bool) -> Field {
  Bool(key:, value:)
}

/// Creates an integer field with the given key and value.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.info("Message", [
///   glogg.int("key", 42),
/// ])
/// ```
pub fn int(key: String, value: Int) -> Field {
  Int(key:, value:)
}

/// Creates a float field with the given key and value.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.info("Message", [
///   glogg.float("key", 3.14),
/// ])
/// ```
pub fn float(key: String, value: Float) -> Field {
  Float(key:, value:)
}

/// Creates a duration field in milliseconds with the given key and value.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.info("Message", [
///   glogg.duration_ms("key", duration.milliseconds(10)),
/// ])
/// ```
pub fn duration_ms(key: String, duration: Duration) -> Field {
  Float(key:, value: duration.to_seconds(duration) *. 1000.0)
}

/// Creates a group field with the given key and nested fields.
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.info("Message", [
///   glogg.group("key", [
///     glogg.string("nested_key", "nested_value"),
///   ])
/// ])
/// ```
pub fn group(key: String, fields: List(Field)) -> Field {
  Group(key:, fields:)
}

/// Creates a stacktrace field that with the key "stacktrace".
///
/// # Example
///
/// ```gleam
/// logger
/// |> glogg.info("Message", [
///   glogg.stacktrace(),
/// ])
/// ```
pub fn stacktrace() -> Field {
  Stacktrace
}

fn dict_to_dynamic(dict: Dict(String, Dynamic)) -> Dynamic {
  let pairs =
    dict
    |> dict.to_list()
    |> list.map(fn(pair) { #(dynamic.string(pair.0), pair.1) })

  dynamic.properties(pairs)
}

/// Converts a list of fields into a metadata dictionary suitable for logging.
pub fn fields_to_metadata(fields: List(Field)) -> Dict(String, Dynamic) {
  list.fold(fields, dict.new(), fn(acc, field) {
    case field {
      String(k, v) -> dict.insert(acc, k, dynamic.string(v))
      Bool(k, v) -> dict.insert(acc, k, dynamic.bool(v))
      Int(k, v) -> dict.insert(acc, k, dynamic.int(v))
      Float(k, v) -> dict.insert(acc, k, dynamic.float(v))
      Group(k, fs) ->
        dict.insert(acc, k, fields_to_metadata(fs) |> dict_to_dynamic)
      Stacktrace ->
        dict.insert(
          acc,
          "stacktrace",
          dynamic.array(platform_capture_stacktrace()),
        )
    }
  })
}

fn log(logger: Logger, level: Level, message: String, fields: List(Field)) {
  let all_fields = list.append(logger.context, fields)
  let metadata = fields_to_metadata(all_fields)
  platform_log(level_to_string(level), message, metadata)
}
