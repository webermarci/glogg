import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/time/duration
import gleeunit
import gleeunit/should
import glogg/handler
import glogg/level
import glogg/logger.{
  type LogEvent, bool, duration_ms, fields_to_metadata, float, get_context,
  group, int, stacktrace, string, with_context,
}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn handler_test() {
  let my_handler =
    handler.new("test_handler")
    |> handler.with_minimal_level(level.Warning)

  should.equal(handler.get_id(my_handler), "test_handler")

  handler.add(my_handler)
  handler.remove("test_handler")
}

pub fn logger_context_test() {
  let logger =
    logger.new("test")
    |> with_context([
      string("app", "glogg_test"),
      string("env", "test"),
    ])

  should.equal(list.length(logger |> get_context), 3)

  should.be_true(list.contains(logger |> get_context, string("logger", "test")))

  should.be_true(list.contains(
    logger |> get_context,
    string("app", "glogg_test"),
  ))

  should.be_true(list.contains(logger |> get_context, string("env", "test")))

  let logger =
    logger
    |> with_context([
      int("version", 1),
    ])

  should.equal(list.length(logger |> get_context), 4)

  should.be_true(list.contains(logger |> get_context, int("version", 1)))
}

pub fn fields_to_metadata_test() {
  let metadata =
    fields_to_metadata([
      string("string_key", "value"),
      int("int_key", 42),
      bool("bool_key", True),
      float("float_key", 3.14),
      duration_ms("duration_key", duration.milliseconds(10)),
      group("group_key", [
        string("nested_string_key", "nested_value"),
        int("nested_int_key", 1337),
      ]),
      stacktrace(),
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

pub fn logging_test() {
  handler.setup_default_handler()

  let assert_and_cancel_hook = fn(event: LogEvent) {
    should.be_true(list.contains(event.fields, string("logger", "test_logger")))
    should.be_true(list.contains(event.fields, string("env", "test")))
    should.be_true(list.contains(event.fields, string("hello", "world")))
    None
  }

  let logger =
    logger.new("test_logger")
    |> with_context([string("env", "test")])
    |> logger.add_hook(assert_and_cancel_hook)

  logger
  |> logger.info("test", [string("hello", "world")])
}
