import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/time/duration
import gleeunit
import gleeunit/should
import glogg

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn default_fields_test() {
  let logger =
    glogg.new()
    |> glogg.with_default_fields([
      glogg.string("app", "glogg_test"),
      glogg.string("env", "test"),
    ])

  should.equal(list.length(glogg.get_default_fields(logger)), 2)

  let logger =
    logger
    |> glogg.add_default_fields([
      glogg.int("version", 1),
    ])

  should.equal(list.length(glogg.get_default_fields(logger)), 3)
}

pub fn fields_to_metadata_test() {
  let metadata =
    glogg.fields_to_metadata([
      glogg.string("string_key", "value"),
      glogg.int("int_key", 42),
      glogg.bool("bool_key", True),
      glogg.float("float_key", 3.14),
      glogg.duration_ms("duration_key", duration.milliseconds(10)),
      glogg.group("group_key", [
        glogg.string("nested_string_key", "nested_value"),
        glogg.int("nested_int_key", 1337),
      ]),
      glogg.stacktrace(),
    ])

  should.equal(dict.size(metadata), 7)

  use string_value <- result.try(dict.get(metadata, "string_key"))
  should.equal(string_value, dynamic.string("value"))

  use int_value <- result.try(dict.get(metadata, "int_key"))
  should.equal(int_value, dynamic.int(42))

  use bool_value <- result.try(dict.get(metadata, "bool_key"))
  should.equal(bool_value, dynamic.bool(True))

  use float_value <- result.try(dict.get(metadata, "float_key"))
  should.equal(float_value, dynamic.float(3.14))

  use duration_value <- result.try(dict.get(metadata, "duration_key"))
  should.equal(duration_value, dynamic.float(10.0))

  use _ <- result.try(dict.get(metadata, "group_key"))

  use stacktrace_value <- result.try(dict.get(metadata, "stacktrace"))
  let stacktrace_decoded =
    decode.run(stacktrace_value, decode.list(decode.string))

  case stacktrace_decoded {
    Ok(stack) -> should.not_equal(stack, [])
    Error(_) -> should.fail()
  }

  Ok(Nil)
}

pub fn log_functions_test() {
  glogg.configure_default_json_formatting()
  glogg.configure_default_minimum_level(glogg.Error)

  let logger =
    glogg.new()
    |> glogg.with_default_fields([
      glogg.string("app", "glogg_test"),
      glogg.string("env", "test"),
    ])

  glogg.debug(logger, "Debug message", [])
  glogg.info(logger, "Info message", [])
  glogg.notice(logger, "Notice message", [])
  glogg.warning(logger, "Warning message", [])
  glogg.error(logger, "Error message", [])
  glogg.critical(logger, "Critical message", [])
  glogg.alert(logger, "Alert message", [])
  glogg.emergency(logger, "Emergency message", [])

  glogg.error(logger, "Stack", [glogg.stacktrace()])

  Ok(Nil)
}
