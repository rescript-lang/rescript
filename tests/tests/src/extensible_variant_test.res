open Mocha
open Test_utils

type attr = ..

type attr += Str(string)

module N = {
  type attr += Int(int, int)
}

type attr += Int(int, int)

let to_int = (x: attr) =>
  switch x {
  | Str(_) => -1
  | N.Int(a, _) => a
  | Int(_, b) => b
  | _ => assert(false)
  }

describe(__MODULE__, () => {
  test("test_int", () => {
    eq(__LOC__, 3, to_int(N.Int(3, 0)))
  })

  test("test_int2", () => {
    eq(__LOC__, 0, to_int(Int(3, 0)))
  })

  test("test_string", () => {
    eq(__LOC__, -1, to_int(Str("x")))
  })
})
