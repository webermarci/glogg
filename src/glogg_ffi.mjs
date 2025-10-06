import { toList } from "../gleam_stdlib/gleam.mjs";

export function captureStacktrace() {
  const err = new Error();
  const stack = err.stack;

  if (!stack) {
    return toList([]);
  }

  return toList(
    stack
      .split("\n")
      .splice(1)
      .map((line) => line.substring(7, line.length).trim()),
  );
}
