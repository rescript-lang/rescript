open Mocha
open Test_utils

@deriving(jsConverter)
type t = [
  | @as("x") #a
  | @as("hi") #u
  | @as(`你`) #b
  | @as(`我`) #c
]

let (v, u) = (tToJs, tFromJs)

/* not applicable to thiis type, and unused warning */

describe(__MODULE__, () => {
  test("jsConverter derivation", () => {
    eq(__LOC__, v(#a), "x")
    eq(__LOC__, v(#u), "hi")
    eq(__LOC__, v(#b), `你`)
    eq(__LOC__, v(#c), `我`)

    eq(__LOC__, u("x"), Some(#a))
    eq(__LOC__, u("hi"), Some(#u))
    eq(__LOC__, u(`你`), Some(#b))
    eq(__LOC__, u(`我`), Some(#c))
    eq(__LOC__, u("xx"), None)
  })
})
