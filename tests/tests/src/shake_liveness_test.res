// Which toplevel bindings Js_shake keeps: exports, bindings with side effects,
// bindings used by statements with side effects, and transitively whatever
// they use. The generated JS shows that only the `dead*` bindings are removed.
open Mocha
open Test_utils

let counter = ref(0)

// A chain that runs backwards through the module: each binding is only needed
// because a later one uses it, and only the last one is exported.
%%private(let chain0 = (x: int) => x * 3 + x * x + 1)
%%private(let chain1 = (x: int) => chain0(x) * 3 + chain0(x + 1) - 2 * x)
%%private(let chain2 = (x: int) => chain1(x) * 3 + chain1(x + 1) - 2 * x)
let chainEnd = (x: int) => chain2(x) + chain2(x + 1)

// Unused, but its initializer has a side effect: it compiles to the statement
// `bump(1)`, which keeps `bump` and, through it, `effectHelper`.
%%private(let effectHelper = (x: int) => x * x * 7 + x * 3 + 5)
%%private(
  let bump = (x: int) => {
    counter := counter.contents + effectHelper(x)
    counter.contents
  }
)
%%private(let effectful = bump(1))

// Only used by a toplevel statement with a side effect.
%%private(let statementHelper = (x: int) => x * x * 5 + x * 11 + 2)
counter := counter.contents + statementHelper(2)

// Unused chain without side effects: removed.
%%private(let deadUsedByDead = (x: int) => x * x * 9 + x * 13 + 4)
%%private(let dead0 = (x: int) => deadUsedByDead(x) * 3 + deadUsedByDead(x + 1))
%%private(let dead1 = (x: int) => dead0(x) * 3 + dead0(x + 1) - x)

describe(__LOC__, () => {
  test("kept bindings work", () => {
    // effectHelper(1) = 15, statementHelper(2) = 44
    eq(__LOC__, counter.contents, 59)
    eq(__LOC__, chainEnd(1), 338)
  })
})
