/* Avoid @val shadowing: `let process = X.process` must not read itself.
   The toplevel `process` binding is in scope in the whole module, so every
   use of the external compiles to `globalThis.process`. */
module X = {
  @val external process: unknown = "process"
}

let process = X.process /* expect `globalThis.process` */
let proc = X.process /* expect `globalThis.process` */

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

/* Nested lexical bindings: a reference is rewritten while it is inside the
 initializer of any binding with its name, and only there. */
module Y = {
  @val external myGlobalA: int = "myGlobalA"
  @val external myGlobalB: int = "myGlobalB"
}

let myGlobalA = (n: int) => {
  let inner = (m: int) => {
    /* inside both initializers: both rewritten */
    let myGlobalB = Y.myGlobalB * m + Y.myGlobalA * n + m * n * 3 + 7
    myGlobalB * myGlobalB + m
  }
  /* back in myGlobalA's initializer only: myGlobalB stays plain */
  let sibling = (m: int) => Y.myGlobalB * m + Y.myGlobalA * n + m * n * 5 + 11
  inner(n) + inner(n + 1) + sibling(n) + sibling(n + 1) + Y.myGlobalA
}

/* outside both initializers: not rewritten */
let useB = (n: int) => Y.myGlobalB * n + n * n * 7 + 13
