# glogg

A simple, structured JSON logging library for Gleam that works across Erlang and JavaScript targets.

*Named after the traditional Scandinavian mulled wine that warms you up during cold winter nights, this logging library aims to bring the same cozy, warming comfort to your debugging sessions.*

[![Package Version](https://img.shields.io/hexpm/v/glogg)](https://hex.pm/packages/glogg)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glogg/)

## Features

- **Structured JSON Logging** - Clean, parseable log output for production systems.
- **Handler-based System** - Configure multiple log destinations with different log levels.
- **Hook Pipelines** - Intercept and modify log events for advanced use cases like redacting sensitive data or dynamic context injection.
- **Type-Safe Fields** - String, Int, Float, Bool, Duration and nested Group fields.
- **Cross-Platform** - Works on both Erlang and JavaScript targets.

## Quick Start

1.  **Add `glogg` to your project:**

```sh
gleam add glogg
```

2.  **Start logging!**

```gleam
import glogg/logger

pub fn main() {
  let my_logger = logger.new("my_app")

  my_logger
  |> logger.info("Hello from glogg!", [
    logger.string("mood", "cozy"),
  ])
}
```

## Basic Configuration

The default handler can be easily configured. This is useful for setting a global log level or enabling JSON formatting on the Erlang target.

```gleam
import glogg/handler
import glogg/level

pub fn main() {
  // For Erlang, this enables JSON output for the default handler.
  // It is a no-op on JavaScript, which always logs structured objects.
  handler.configure_default_handler_json_formatting()

  // Set the minimum log level for the default handler.
  handler.configure_default_handler_minimum_level(level.Info)
}
```

## Fields

You can add structured data to your logs using `Field`s.

```gleam
import gleam/time
import glogg/logger

pub fn handle_request(logger: logger.Logger) {
  logger
  |> logger.info("API request completed", [
    // Add basic fields
    logger.string("method", "POST"),
    logger.int("status", 201),
    logger.bool("is_testing", False),
    logger.float("load_time", 0.256),

    // Add time durations
    logger.duration_ms("duration", time.milliseconds(120)),

    // Add nested groups of fields
    logger.group("user", [
      logger.int("id", 1),
      logger.string("name", "admin"),
    ]),

    // Add a stacktrace on errors
    logger.stacktrace(),
  ])
}
```

## Advanced Usage

### Custom Handlers

While the default handler is useful, you can add your own for more complex scenarios. For example, you might want to log errors to the console, but all other logs elsewhere.

Currently, `glogg` only supports console handlers (`logger_std_h` on Erlang), but the API is designed for future expansion to file handlers.

```gleam
import glogg/handler
import glogg/level
import glogg/logger

pub fn main() {
  // Remove the default handler to take full control.
  handler.remove("default")

  // Add a new handler that only logs warnings and above.
  let warning_handler =
    handler.new("console_warnings")
    |> handler.with_minimal_level(level.Warning)
  handler.add(warning_handler)

  let logger = logger.new("my_app")

  // This message will NOT be logged.
  logger |> logger.info("This is too quiet", [])

  // This message WILL be logged.
  logger |> logger.error("This is loud enough!", [])
}
```

### Hooks (Middleware)

Hooks are a powerful feature that allow you to create a pipeline to process and modify every log event for a given logger. They are especially useful for testing or dynamically adding context.

A hook is a function that takes a `LogEvent` and returns an `Option(LogEvent)`.
- Returning `Some(event)` passes the (potentially modified) event to the next hook in the pipeline.
- Returning `None` cancels the log event entirely.

Hooks are executed in the order they are added.

```gleam
import glogg/logger
import gleam/list
import gleam/option.{Some}

pub fn main() {
  // This hook redacts password fields for security.
  let redact_hook = fn(event: logger.LogEvent) {
    let new_fields =
      list.map(event.fields, fn(field) {
        case field {
          logger.String("password", _) -> logger.string("password", "[REDACTED]")
          _ -> field
        }
      })
    Some(logger.LogEvent(..event, fields: new_fields))
  }

  let logger =
    logger.new("my_app")
    |> logger.add_hook(redact_hook)

  // The final log output will have the `password` field redacted.
  logger
  |> logger.info("User logged in", [
    logger.string("user", "admin"),
    logger.string("password", "supersecret"),
  ])
}
```
