import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/time/duration.{type Duration}
import glogg/level.{
  type Level, Alert, Critical, Debug, Emergency, Error, Info, Notice, Warning,
  level_to_severity, level_to_string,
}

@external(erlang, "logger_ffi", "log")
@external(javascript, "./logger_ffi.mjs", "log")
fn platform_log(
  level: String,
  message: String,
  metadata: Dict(String, dynamic.Dynamic),
) -> Nil

@external(erlang, "logger_ffi", "capture_stacktrace")
@external(javascript, "./logger_ffi.mjs", "captureStacktrace")
fn platform_capture_stacktrace() -> List(Dynamic)

pub type LogEvent {
  LogEvent(level: Level, message: String, fields: List(Field))
}

pub type HookFn =
  fn(LogEvent) -> Option(LogEvent)

pub opaque type Field {
  String(key: String, value: String)
  Bool(key: String, value: Bool)
  Int(key: String, value: Int)
  Float(key: String, value: Float)
  Group(key: String, fields: List(Field))
  Lazy(thunk: fn() -> Field)
  Stacktrace
}

pub opaque type Logger {
  Logger(
    name: String,
    context: List(Field),
    minimum_level: Option(Level),
    hooks: List(HookFn),
  )
}

/// Creates a new logger.
///
/// The logger will have a default context field "logger" with the given name.
///
/// # Example
///
/// ```gleam
/// let my_logger = logger.new("name")
/// ```
pub fn new(name: String) -> Logger {
  Logger(name, [String("logger", name)], None, [])
}

/// Adds context fields to a logger.
///
/// These fields will be included in every log message.
///
/// # Example
///
/// ```gleam
/// let my_logger =
///   logger.new("name")
///   |> logger.with_context([
///     logger.string("app", "my_app"),
///     logger.string("env", "production"),
///   ])
/// ```
pub fn with_context(logger: Logger, fields: List(Field)) -> Logger {
  Logger(..logger, context: list.append(logger.context, fields))
}

/// Retrieves the context fields of a logger.
pub fn get_context(logger: Logger) -> List(Field) {
  logger.context
}

/// Sets the minimum level for this logger.
/// Logs below this level are discarded before processing fields.
///
/// # Example
///
/// ```gleam
/// let my_logger =
///   logger.new("name")
///   |> logger.with_minimum_level(Info)  // Ignore debug logs
/// ```
pub fn with_minimum_level(logger: Logger, level: Level) -> Logger {
  Logger(..logger, minimum_level: Some(level))
}

/// Adds a hook function to a logger.
/// The hook function can modify or filter log events.
/// Hooks are executed in the order they are added.
///
/// If the hook returns `None`, the log event will be discarded.
/// If the hook returns `Some(event)`, the modified event will be logged.
///
/// # Example
///
/// ```gleam
/// let my_logger =
///   logger.new("name")
///   |> logger.add_hook(fn(event) {
///     case event {
///       logger.LogEvent(level, message, fields) ->
///         // Add a custom field to every log event
///         let new_fields =
///           list.append(fields, [
///             logger.string("custom_field", "value")
///           ])
///         Some(logger.LogEvent(level, message, new_fields))
///     }
///   })
/// ```
pub fn add_hook(logger: Logger, hook: HookFn) -> Logger {
  Logger(..logger, hooks: list.append(logger.hooks, [hook]))
}

/// Clears all hooks from a logger.
pub fn clear_hooks(logger: Logger) -> Logger {
  Logger(..logger, hooks: [])
}

/// Logs a message and fields at the `Debug` level.
///
/// # Example
///
/// ```gleam
/// my_logger
/// |> logger.debug("Message", [
///   logger.string("key", "value"),
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
/// my_logger
/// |> logger.info("Message", [
///   logger.string("key", "value"),
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
/// my_logger
/// |> logger.notice("Message", [
///   logger.string("key", "value"),
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
/// my_logger
/// |> logger.warning("Message", [
///   logger.string("key", "value"),
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
/// my_logger
/// |> logger.error("Message", [
///   logger.string("key", "value"),
///   logger.stacktrace(),
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
/// my_logger
/// |> logger.critical("Message", [
///   logger.string("key", "value"),
///   logger.stacktrace(),
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
/// my_logger
/// |> logger.alert("Message", [
///   logger.string("key", "value"),
///   logger.stacktrace(),
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
/// my_logger
/// |> logger.emergency("Message", [
///   logger.string("key", "value"),
///   logger.stacktrace(),
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
/// my_logger
/// |> logger.info("Message", [
///   logger.string("key", "value"),
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
/// my_logger
/// |> logger.info("Message", [
///   logger.bool("key", True),
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
/// my_logger
/// |> logger.info("Message", [
///   logger.int("key", 42),
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
/// my_logger
/// |> logger.info("Message", [
///   logger.float("key", 3.14),
/// ])
/// ```
pub fn float(key: String, value: Float) -> Field {
  Float(key:, value:)
}

/// Creates a duration field with the given key and value.
///
/// # Example
///
/// ```gleam
/// my_logger
/// |> logger.info("Message", [
///   logger.duration("key", duration.milliseconds(10)),
/// ])
/// ```
pub fn duration(key: String, duration: Duration) -> Field {
  Float(key:, value: duration.to_seconds(duration) *. 1000.0)
}

/// Creates a group field with the given key and nested fields.
///
/// # Example
///
/// ```gleam
/// my_logger
/// |> logger.info("Message", [
///   logger.group("key", [
///     logger.string("nested_key", "nested_value"),
///   ])
/// ])
/// ```
pub fn group(key: String, fields: List(Field)) -> Field {
  Group(key:, fields:)
}

/// Wraps a field to be evaluated only when logging actually occurs.
///
/// # Example
///
/// ```gleam
/// logger.debug("Query result", [
///   logger.string("query", "SELECT ..."),
///   logger.lazy(fn() {
///     logger.string("result", expensive_json_encode(data))
///   }),
/// ])
/// ```
pub fn lazy(thunk: fn() -> Field) -> Field {
  Lazy(thunk:)
}

/// Creates a stacktrace field that with the key "stacktrace".
///
/// # Example
///
/// ```gleam
/// my_logger
/// |> logger.info("Message", [
///   logger.stacktrace(),
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
      Lazy(thunk) -> {
        dict.fold(
          fields_to_metadata([thunk()]),
          acc,
          fn(result_acc, key, value) { dict.insert(result_acc, key, value) },
        )
      }
      Stacktrace ->
        dict.insert(
          acc,
          "stacktrace",
          dynamic.array(platform_capture_stacktrace()),
        )
    }
  })
}

fn should_log(minimum_level: Option(Level), level: Level) -> Bool {
  case minimum_level {
    None -> True
    Some(min) -> level_to_severity(level) <= level_to_severity(min)
  }
}

fn log(logger: Logger, level: Level, message: String, fields: List(Field)) {
  case should_log(logger.minimum_level, level) {
    False -> Nil
    True -> log_internal(logger, level, message, fields)
  }
}

fn apply_hooks(hooks: List(HookFn), event: LogEvent) -> Option(LogEvent) {
  case hooks {
    [] -> Some(event)
    [hook, ..rest] ->
      case hook(event) {
        None -> None
        Some(new_event) -> apply_hooks(rest, new_event)
      }
  }
}

fn log_internal(
  logger: Logger,
  level: Level,
  message: String,
  fields: List(Field),
) {
  let all_fields = list.append(logger.context, fields)
  let initial_event = LogEvent(level, message, all_fields)

  case apply_hooks(logger.hooks, initial_event) {
    Some(final_event) -> {
      platform_log(
        level_to_string(final_event.level),
        final_event.message,
        fields_to_metadata(final_event.fields),
      )
    }
    None -> Nil
  }
}
