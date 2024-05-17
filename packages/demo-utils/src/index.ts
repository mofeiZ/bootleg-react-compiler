export * from "./logger";
export * from "./babel-helpers";
export * from "./DisjointSet";

export function getFiltered<T>(m: Map<T, boolean>): Array<T> {
  return Array.from(m.entries())
    .filter(([_, isOk]) => isOk)
    .map(([key, _]) => key);
}
