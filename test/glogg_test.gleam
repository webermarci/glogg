import gleam/string
import gleeunit
import gleeunit/should
import glogg

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn serialization_test() {
  let level = glogg.Info
  let message = "test"
  let default_fields = [
    glogg.string("default", "default"),
  ]
  let fields = [
    glogg.string("string", "value"),
    glogg.int("int", 42),
    glogg.bool("bool", True),
    glogg.float("float", 3.14),
    glogg.group("group", [
      glogg.string("embedded_string", "value"),
      glogg.bool("embedded_bool", True),
      glogg.int("embedded_int", 42),
      glogg.float("embedded_float", 3.14),
    ]),
  ]

  let serialized = glogg.serialize(level, message, default_fields, fields)

  should.be_true(string.starts_with(serialized, "{"))
  should.be_true(string.ends_with(serialized, "}"))
  should.be_true(string.contains(serialized, "\"level\":\"INFO\""))
  should.be_true(string.contains(serialized, "\"msg\":\"test\""))
  should.be_true(string.contains(serialized, "\"default\":\"default\""))
  should.be_true(string.contains(serialized, "\"string\":\"value\""))
  should.be_true(string.contains(serialized, "\"int\":42"))
  should.be_true(string.contains(serialized, "\"bool\":true"))
  should.be_true(string.contains(serialized, "\"float\":3.14"))
  should.be_true(string.contains(
    serialized,
    "\"group\":{\"embedded_string\":\"value\",\"embedded_bool\":true,\"embedded_int\":42,\"embedded_float\":3.14}",
  ))
}

pub fn stacktrace_test() {
  glogg.capture_stacktrace()
  |> should.not_equal([])
}
