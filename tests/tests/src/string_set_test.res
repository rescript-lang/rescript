open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("String_set cardinality", () => {
    let number = 1_000_00
    let s = ref(String_set.empty)
    for i in 0 to number - 1 {
      s := String_set.add(Js.Int.toString(i), s.contents)
    }
    eq(__LOC__, String_set.cardinal(s.contents), number)
  })
})
