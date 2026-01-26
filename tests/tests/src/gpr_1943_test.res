open Mocha
open Test_utils

let f = x => (x["_003"], x["_50"], x["_50x"], x["__50"], x["__50x"], x["_50x'"], x["x'"])
/* x##_ */ /* TODO: should have a syntax error */

describe(__MODULE__, () => {
  test("object property access with special characters", () => {
    let v = f({
      "_003": 0,
      "_50": 1,
      "_50x": 2,
      "__50": 3,
      "__50x": 4,
      "_50x'": 5,
      "x'": 6,
      /* _  = 6 */
    })
    eq(__LOC__, (0, 1, 2, 3, 4, 5, 6), v)
  })
})
