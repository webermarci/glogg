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

/// Converts a log level to its string representation.
pub fn level_to_string(level: Level) -> String {
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

/// Converts a log level to its corresponding syslog severity number.
pub fn level_to_severity(level: Level) -> Int {
  case level {
    Debug -> 7
    Info -> 6
    Notice -> 5
    Warning -> 4
    Error -> 3
    Critical -> 2
    Alert -> 1
    Emergency -> 0
  }
}
