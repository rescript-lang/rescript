// Keep Node rendering tests on the same runtime as vite.config.js.
export function resolve(specifier, context, nextResolve) {
  if (specifier.startsWith("@rescript/runtime/")) {
    return nextResolve(
      new URL(`../node_modules/${specifier}`, import.meta.url).href,
      context,
    );
  }
  return nextResolve(specifier, context);
}
