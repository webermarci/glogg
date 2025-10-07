# glogg

A simple, structured JSON logging library for Gleam that works across Erlang and JavaScript targets.

*Named after the traditional Scandinavian mulled wine that warms you up during cold winter nights, this logging library aims to bring the same cozy, warming comfort to your debugging sessions.*

[![Package Version](https://img.shields.io/hexpm/v/glogg)](https://hex.pm/packages/glogg)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glogg/)

## Features

- **Structured JSON Logging** - Clean, parseable log output
- **Type-Safe Fields** - String, Int, Float, Bool, Duration and nested Group fields
- **Stacktrace** - Capture and log stacktraces
- **Cross-Platform** - Works on both Erlang and JavaScript targets

## Quick Start

```sh
gleam add glogg
```

```gleam
import glogg.{bool, info, int, string}

pub fn main() {
  glogg.configure_default_json_formatting()
  glogg.configure_default_minimum_level(glogg.Debug)

  glogg.new()
  |> info("this is fine", [
    bool("everything_burning", True),
    string("glogg_temperature", "still_hot"),
    int("production_issues", 42069)
  ])
}
```

## Usage Examples

### Structured Fields

```gleam
info(logger, "processing payment", [
  string("payment_id", "pay_123"),
  float("amount", 29.99),
  int("user_id", 12345),
  bool("verified", True),
  duration_ms("processing_time", duration.millisecond(150)
])
```

#### Stacktrace

```gleam
error(logger, "huge issue", [
  string("payment_id", "pay_123"),
  int("user_id", 12345),
  stacktrace()
])
```

### Nested Fields

```gleam
info(logger, "api request completed", [
  group("request", [
    string("method", "POST"),
    string("path", "/api/users"),
    int("status", 201)
  ]),
  group("response", [
    float("duration_ms", 45.2),
    int("bytes", 1024)
  ])
])
```

### Logger Configuration

```gleam
// Configures the BEAM logger to output JSON to stdout.
// This should be done once at the start of your application.
// It is a no-op on JavaScript targets.
glogg.configure_default_json_formatting()

// Set the minimum log level
// (Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency)
glogg.configure_default_minimum_level(glogg.Info)

// Add default fields to all logs
let logger =
  glogg.new()
  |> with_default_fields([
    string("service", "web-api"),
    string("version", "1.2.3"),
  ])

// Add more default fields later
let logger = glogg.add_default_fields(logger, [
  string("environment", "production")
])
```
