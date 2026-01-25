open Mocha
open Test_utils

%%raw(`
function sum(a,b){
  return a + b
}
`)

external sum: (float, float) => float = "sum"

describe(__MODULE__, () => {
  test("uncurried external function call", () => {
    let h = sum(1.0, 2.0)
    eq(__LOC__, h, 3.)
  })
})
