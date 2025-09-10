open Mocha
open Test_utils

let f = h => h["x"]["y"]["z"]

let f2 = h => h["x"]["y"]["z"]

let f3 = (h, x, y) => h["paint"](x, y)["draw"](x, y)

let f4 = (h, x, y) => {
  h["paint"] = (x, y)
  h["paint"]["draw"] = (x, y)
}

/* let g h = */
/* h##(draw (x,y)) */
/* ##(draw (x,y)) */
/* ##(draw(x,y)) */
describe(__MODULE__, () => {
  test("chain code test", () => {
    eq(__LOC__, 32, f2({"x": {"y": {"z": 32}}}))
  })
})
