# glogg

A simple, structured JSON logging library for Gleam that works across Erlang and JavaScript targets.

*Named after the traditional Scandinavian mulled wine that warms you up during cold winter nights, this logging library aims to bring the same cozy, warming comfort to your debugging sessions.*

[![Package Version](https://img.shields.io/hexpm/v/glogg)](https://hex.pm/packages/glogg)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glogg/)

## Features

- **Structured JSON Logging** - Clean, parseable log output
- **Type-Safe Fields** - String, Int, Float, Bool, and nested Group fields
- **Flexible Configuration** - Custom writers, default fields, and minimum levels
- **Cross-Platform** - Works on both Erlang and JavaScript targets

## Quick Start

```sh
gleam add glogg
```

```gleam
import glogg.{bool, info, int, string}

pub fn main() {
  let logger = glogg.new()

  info(logger, "This is fine", [
    bool("everything_burning", True),
    string("glogg_temperature", "still_hot"),
    int("production_issues", 42069)
  ])
}
```

**Output:**

```json
{"time":"2025-09-25T22:03:45.124Z","level":"INFO","msg":"This is fine","everything_burning":true,"glogg_temperature":"still_hot","production_issues":42069}
```

## Usage Examples

### Structured Fields

```gleam
info(logger, "Processing payment", [
  string("payment_id", "pay_123"),
  float("amount", 29.99),
  int("user_id", 12345),
  bool("verified", True)
])
```

### Nested Fields

```gleam
info(logger, "API request completed", [
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
// Set minimum log level
let logger =
  glogg.new()
  |> with_min_level(Info)
// Only Info, Warn, Error

// Add default fields to all logs
let logger =
  glogg.new()
  |> with_default_fields([
    string("service", "web-api"),
    string("version", "1.2.3"),
  ])

// Add more default fields later
let logger =
  logger
  |> add_default_fields([string("environment", "production")])
```

### Custom Writers

```gleam
// Custom writer function
let file_writer = fn(line: String) {
  // Write to file, send to external service, etc.
  io.println("LOG: " <> line)
}

let logger =
  glogg.new()
  |> with_writer(file_writer)
  |> with_error_writer(file_writer) // Separate error writer
```
