open Mocha
open Test_utils

let v = 2->(3->\"+")

module X = {
  type t = Some(int)
}

let u = 3->X.Some

let xx = (obj, a0, a1, a2, a3, a4, a5) => obj->a0(a1)->a2(a3)->a4(a5)->\"-"(1)->(3->\"-")
/*
  (a4 (a2 (a0 obj a1) a3) a5)
*/

describe(__MODULE__, () => {
  test("method chaining with + operator", () => {
    eq(__LOC__, v, 5)
  })

  test("complex method chaining", () => {
    eq(__LOC__, xx(3, \"-", 2, \"+", 4, \"*", 3), 11)
  })
})
