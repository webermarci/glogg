import { toList } from "../../gleam_stdlib/gleam.mjs";
import { handlers } from "./handler_ffi.mjs";

const isDeno = typeof Deno !== "undefined";
const isBun = typeof Bun !== "undefined";
const isNode =
  typeof process !== "undefined" &&
  process.versions != null &&
  process.versions.node != null;
const isServer = isNode || isDeno || isBun;

const severity = {
  emergency: 0,
  alert: 1,
  critical: 2,
  error: 3,
  warning: 4,
  notice: 5,
  info: 6,
  debug: 7,
};

function write(level, data) {
  const isError =
    level === "error" ||
    level === "critical" ||
    level === "alert" ||
    level === "emergency";

  if (isServer) {
    const jsonString = JSON.stringify(data) + "\n";

    if (isNode || isBun) {
      const stream = isError ? process.stderr : process.stdout;
      stream.write(jsonString);
    } else if (isDeno) {
      const stream = isError ? Deno.stderr : Deno.stdout;
      stream.writeSync(new TextEncoder().encode(jsonString));
    }
  } else {
    if (isError) {
      console.error(data);
    } else if (level === "warning") {
      console.warn(data);
    } else if (level === "info" || level === "notice") {
      console.info(data);
    } else if (level === "debug") {
      console.debug(data);
    } else {
      console.log(data);
    }
  }
}

export function captureStacktrace() {
  // Use V8's captureStackTrace when available to omit this function from frames
  const holder = {};
  if (typeof Error.captureStackTrace === "function") {
    Error.captureStackTrace(holder, captureStacktrace);
  } else {
    try {
      throw new Error();
    } catch (e) {
      holder.stack = e && e.stack ? String(e.stack) : "";
    }
  }

  const stack = holder.stack || "";
  if (!stack) return toList([]);

  // Normalize to a list of trimmed frames (skip the first line/header)
  const frames = stack
    .split("\n")
    .slice(1)
    .map((l) => l.trim());
  return toList(frames);
}

function deepGleamDictToObject(data) {
  if (
    typeof data === "object" &&
    data !== null &&
    data.entries &&
    !Array.isArray(data)
  ) {
    const obj = {};

    for (const [key, value] of data.entries()) {
      obj[key] = deepGleamDictToObject(value);
    }

    return obj;
  }

  return data;
}

export function log(level, message, metadata) {
  const messageSeverity = severity[level];

  if (messageSeverity === undefined || handlers.size === 0) {
    return;
  }

  const filteredHandlers = handlers
    .values()
    .filter((h) => severity[h.minimum_level] > messageSeverity)
    .toArray();

  if (!filteredHandlers.length) {
    return;
  }

  const metadataObject = deepGleamDictToObject(metadata);

  const data = {
    time: new Date().getTime() * 1000,
    level: level,
    msg: message,
    ...metadataObject,
  };

  write(level, data);
}
