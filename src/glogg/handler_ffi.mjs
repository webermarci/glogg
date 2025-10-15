export const handlers = new Map([["default", { minimum_level: "notice" }]]);

export function setDefaultHandlerJsonFormatting(handler) {
  return;
}

export function setDefaultHandlerMinimumLevel(level) {
  const defaultHandler = handlers.get("default");
  if (defaultHandler) {
    defaultHandler.minimum_level = level;
  }
}

export function setPrimaryMinimumLevel(level) {
  return;
}

export function addHandler(id, level) {
  handlers.set(id, { minimum_level: level });
}

export function setHandlerMinimumLevel(id, level) {
  handlers.set(id, { ...handlers.get(id), minimum_level: level });
}

export function removeHandler(id) {
  handlers.delete(id);
}
