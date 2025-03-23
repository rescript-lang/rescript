/**
 * @param {string} s
 */
export function normalizeNewlines(s) {
  return s.replace(/\r\n/g, "\n");
}
