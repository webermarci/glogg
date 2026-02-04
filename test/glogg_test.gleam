import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/time/duration
import gleeunit
import gleeunit/should
import glogg/handler
import glogg/level
import glogg/logger.{
  type LogEvent, bool, duration, fields_to_metadata, float, get_context, group,
  int, lazy, stacktrace, string, with_context,
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
      duration("duration_key", duration.milliseconds(10)),
      lazy(fn() { string("lazy_string_key", "value") }),
      group("group_key", [
        string("nested_string_key", "nested_value"),
        int("nested_int_key", 1337),
      ]),
      stacktrace(),
    ])

  should.equal(dict.size(metadata), 8)

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

  use _ <- result.try(dict.get(metadata, "lazy_string_key"))

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

pub fn lazy_evaluation_test() {
  let assert_hook = fn(event: LogEvent) { Some(event) }
  let cancel_hook = fn(_event: LogEvent) { None }

  let logger =
    logger.new("test")
    |> logger.add_hook(assert_hook)
    |> logger.add_hook(cancel_hook)

  logger
  |> logger.info("test", [
    logger.lazy(fn() {
      should.fail()
      string("never", "executed")
    }),
  ])
}

pub fn level_filtering_test() {
  let logger =
    logger.new("test")
    |> logger.with_minimum_level(level.Warning)

  logger
  |> logger.debug("should be filtered", [
    logger.lazy(fn() {
      should.fail()
      string("not", "logged")
    }),
  ])

  let logger =
    logger
    |> logger.add_hook(fn(event: LogEvent) {
      should.equal(event.level, level.Warning)
      None
    })

  logger
  |> logger.warning("should pass", [])
}

pub fn hook_chain_test() {
  let add_field_1 = fn(event: LogEvent) {
    let new_fields = list.append(event.fields, [string("hook1", "ran")])
    Some(logger.LogEvent(..event, fields: new_fields))
  }

  let add_field_2 = fn(event: LogEvent) {
    let new_fields = list.append(event.fields, [string("hook2", "ran")])
    Some(logger.LogEvent(..event, fields: new_fields))
  }

  let assert_hook = fn(event: LogEvent) {
    should.be_true(list.contains(event.fields, string("hook1", "ran")))
    should.be_true(list.contains(event.fields, string("hook2", "ran")))
    None
  }

  let logger =
    logger.new("test")
    |> logger.add_hook(add_field_1)
    |> logger.add_hook(add_field_2)
    |> logger.add_hook(assert_hook)

  logger |> logger.info("test", [])
}

pub fn hook_short_circuit_test() {
  let cancel_hook = fn(_event: LogEvent) { None }

  let should_not_run = fn(_event: LogEvent) {
    should.fail()
    None
  }

  let logger =
    logger.new("test")
    |> logger.add_hook(cancel_hook)
    |> logger.add_hook(should_not_run)

  logger |> logger.info("test", [])
}

pub fn hook_modification_test() {
  let modify_level = fn(event: LogEvent) {
    Some(logger.LogEvent(..event, level: level.Critical))
  }

  let assert_modified = fn(event: LogEvent) {
    should.equal(event.level, level.Critical)
    None
  }

  let logger =
    logger.new("test")
    |> logger.add_hook(modify_level)
    |> logger.add_hook(assert_modified)

  logger |> logger.info("test", [])
}

pub fn clear_hooks_test() {
  let hook = fn(_event: LogEvent) {
    should.fail()
    None
  }

  let logger =
    logger.new("test")
    |> logger.add_hook(hook)
    |> logger.clear_hooks()

  logger |> logger.info("test", [])
}
