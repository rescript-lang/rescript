/**
 * `import.meta.dirname` alternative. It is available since Node.js v20.11.0
 *
 * @param {string} url `import.meta.url`
 * @return {string}
 */
export function getDirname(url) {
  return path.dirname(fileURLToPath(url));
}

/**
 * @param {string} s
 */
export function normalizeNewlines(s) {
  return s.replace(/\r\n/g, "\n");
}
