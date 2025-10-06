import gleam/io
import gleam/json.{type Json}
import gleam/list
import gleam/time/duration.{type Duration}
import gleam/time/timestamp

pub opaque type Logger {
  Logger(
    min_level: Level,
    default_fields: List(Field),
    writer: Writer,
    error_writer: Writer,
  )
}

pub type Level {
  Trace
  Info
  Error
  Warn
  Debug
}

pub opaque type Field {
  String(key: String, value: String)
  StringList(key: String, value: List(String))
  Bool(key: String, value: Bool)
  Int(key: String, value: Int)
  Float(key: String, value: Float)
  Group(key: String, fields: List(Field))
}

pub type Writer =
  fn(String) -> Nil

/// Creates a new logger with sensible default settings.
/// By default, the minimum level is `Debug` and there are no default fields.
pub fn new() -> Logger {
  Logger(Debug, [], io.println, io.println_error)
}

/// Returns a new logger with the specified minimum log level.
///
/// Log messages with a severity below this level will be ignored.
pub fn with_min_level(logger: Logger, minimum_level: Level) -> Logger {
  Logger(
    minimum_level,
    logger.default_fields,
    logger.writer,
    logger.error_writer,
  )
}

/// Returns a new logger with a custom writer.
///
/// This writer will be used to write all log messages produced by this logger.
pub fn with_writer(logger: Logger, writer: Writer) -> Logger {
  Logger(logger.min_level, logger.default_fields, writer, logger.error_writer)
}

/// Returns a new logger with a custom error writer.
///
/// This writer will be used to write all error log messages produced by this logger.
pub fn with_error_writer(logger: Logger, error_writer: Writer) -> Logger {
  Logger(logger.min_level, logger.default_fields, logger.writer, error_writer)
}

/// Returns a new logger with a set of default fields.
///
/// These fields will be included in every log message produced by this logger,
/// in addition to any fields provided at the call site.
pub fn with_default_fields(logger: Logger, fields: List(Field)) -> Logger {
  Logger(logger.min_level, fields, logger.writer, logger.error_writer)
}

/// Adds additional default fields to an existing logger.
///
/// The new fields are appended to the logger's existing default fields.
/// These fields will be included in every log message produced by the returned logger.
pub fn add_default_fields(logger: Logger, fields: List(Field)) -> Logger {
  Logger(
    logger.min_level,
    list.append(logger.default_fields, fields),
    logger.writer,
    logger.error_writer,
  )
}

/// Logs a message and fields at the `Trace` level.
pub fn trace(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Trace, message, fields)
}

/// Logs a message and fields at the `Debug` level.
pub fn debug(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Debug, message, fields)
}

/// Logs a message and fields at the `Info` level.
pub fn info(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Info, message, fields)
}

/// Logs a message and fields at the `Warn` level.
pub fn warn(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Warn, message, fields)
}

/// Logs a message and fields at the `Error` level to standard error.
pub fn error(logger: Logger, message: String, fields: List(Field)) {
  log(logger, Error, message, fields)
}

/// Creates a string field with the given key and value.
pub fn string(key: String, value: String) -> Field {
  String(key:, value:)
}

/// Creates a boolean field with the given key and value.
pub fn bool(key: String, value: Bool) -> Field {
  Bool(key:, value:)
}

/// Creates an integer field with the given key and value.
pub fn int(key: String, value: Int) -> Field {
  Int(key:, value:)
}

/// Creates a float field with the given key and value.
pub fn float(key: String, value: Float) -> Field {
  Float(key:, value:)
}

/// Creates a group field containing nested fields.
pub fn group(key: String, fields: List(Field)) -> Field {
  Group(key:, fields:)
}

/// Creates a duration field in miliseconds.
pub fn duration_ms(key: String, duration: Duration) -> Field {
  Float(key:, value: duration.to_seconds(duration) *. 1000.0)
}

/// Creates a stacktrace field capturing the current stack trace.
pub fn stacktrace() -> Field {
  StringList("stacktrace", capture_stacktrace())
}

fn level_to_int(level: Level) -> Int {
  case level {
    Trace -> 0
    Debug -> 1
    Info -> 2
    Warn -> 3
    Error -> 4
  }
}

fn level_to_string(level: Level) -> String {
  case level {
    Trace -> "TRACE"
    Debug -> "DEBUG"
    Info -> "INFO"
    Warn -> "WARN"
    Error -> "ERROR"
  }
}

fn log(logger: Logger, level: Level, message: String, fields: List(Field)) {
  case level_to_int(level) < level_to_int(logger.min_level) {
    True -> Nil
    False -> {
      case level {
        Error -> {
          serialize(level, message, logger.default_fields, fields)
          |> logger.error_writer
        }
        _ -> {
          serialize(level, message, logger.default_fields, fields)
          |> logger.writer
        }
      }
    }
  }
}

pub fn serialize(
  level: Level,
  message: String,
  default_fields: List(Field),
  fields: List(Field),
) -> String {
  let initial_fields = [
    #(
      "time",
      json.string(
        timestamp.system_time() |> timestamp.to_rfc3339(duration.seconds(0)),
      ),
    ),
    #("level", json.string(level_to_string(level))),
    #("msg", json.string(message)),
  ]

  let all_user_fields = list.append(default_fields, fields)

  initial_fields
  |> list.append(fields_to_json(all_user_fields))
  |> json.object()
  |> json.to_string()
}

fn fields_to_json(fields: List(Field)) -> List(#(String, Json)) {
  list.map(fields, fn(field) {
    case field {
      String(k, v) -> #(k, json.string(v))
      StringList(k, v) -> #(k, json.array(v, json.string))
      Bool(k, v) -> #(k, json.bool(v))
      Int(k, v) -> #(k, json.int(v))
      Float(k, v) -> #(k, json.float(v))
      Group(k, v) -> #(k, json.object(fields_to_json(v)))
    }
  })
}

@external(erlang, "glogg_ffi", "capture_stacktrace")
@external(javascript, "./glogg_ffi.mjs", "captureStacktrace")
pub fn capture_stacktrace() -> List(String)
