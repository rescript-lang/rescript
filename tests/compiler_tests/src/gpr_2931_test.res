open Mocha
open Test_utils

let fake_c2 = (a_type, b_type) =>
  switch (a_type, b_type) {
  | ("undefined", _) => -1
  | (_, "undefined") => 1
  | ("string", _) => 1
  | ("number", "number") => 33
  | ("number", _) => 3
  | _ => 0
  }

describe(__MODULE__, () => {
  test("fake comparison function", () => {
    eq(__LOC__, 3, fake_c2("number", "xx"))
  })
})
