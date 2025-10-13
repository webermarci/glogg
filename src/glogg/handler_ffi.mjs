export const handlers = new Map([["default", { minimum_level: "notice" }]]);

export function configureDefaultHandlerJsonFormatting(handler) {
  return;
}

export function configureDefaultHandlerMinimumLevel(level) {
  const defaultHandler = handlers.get("default");
  if (defaultHandler) {
    defaultHandler.minimum_level = level;
  }
}

export function addHandler(id, level) {
  handlers.set(id, { minimum_level: level });
}

export function removeHandler(id) {
  handlers.delete(id);
}
