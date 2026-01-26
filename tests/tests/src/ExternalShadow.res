/* Avoid @val shadowing: `let process = X.process` must not read itself.
   The initializer should compile to `globalThis.process`, while other
   uses of the external stay as plain `process`. */
module X = {
  @val external process: unknown = "process"
}

let process = X.process /* expect `globalThis.process` */
let proc = X.process /* expect plain `process` */

/* @new does not have the same shadowing issue because it uses `new URL(...)`. */
module New = {
  @new external url: string => unknown = "URL"
}

let url = New.url /* expect `new URL(...)` in a wrapper function */

/* Reserved JS globals should not be rewritten to globalThis. */
module Global = {
  @val external parseInt: unknown = "parseInt"
}

let parseInt = Global.parseInt /* expect plain `parseInt` */
