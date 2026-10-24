open Mocha
open Test_utils

// break and continue in the arms of an untagged variant switch leave or
// continue the loop, not the generated switch

@unboxed
type value =
  | @as("stop") Stop
  | @as("skip") Skip
  | Date(Date.t)
  | Text(string)
  | Count(float)
  | Object({x: int})

let scan = (values: array<value>) => {
  let seen = []
  for i in 0 to Array.length(values) - 1 {
    switch values->Array.getUnsafe(i) {
    | Stop => break
    | Skip => continue
    | Text("break") => break
    | Text("continue") => continue
    | Text(text) => seen->Array.push(text)
    | Count(n) if n < 0. => break
    | Count(_) => seen->Array.push("count")
    | Date(_) => continue
    | Object(_) => break
    }
    seen->Array.push(".")
  }
  seen
}

// Some cases go to the default, which lands both in the typeof switch and
// in the class chain
let scanDefault = (values: array<value>) => {
  let seen = []
  let i = ref(0)
  while i.contents < Array.length(values) {
    let value = values->Array.getUnsafe(i.contents)
    i := i.contents + 1
    switch value {
    | Stop => break
    | Text(text) => seen->Array.push(text)
    | Date(_) => seen->Array.push("date")
    | _ => continue
    }
    seen->Array.push(".")
  }
  seen
}

let scanDefaultBreak = (values: array<value>) => {
  let seen = []
  for i in 0 to Array.length(values) - 1 {
    switch values->Array.getUnsafe(i) {
    | Text(text) => seen->Array.push(text)
    | Date(_) => seen->Array.push("date")
    | _ => break
    }
  }
  seen
}

describe(__MODULE__, () => {
  test("a literal arm", () => {
    eq(__LOC__, scan([Text("a"), Skip, Text("b"), Stop, Text("c")]), ["a", ".", "b", "."])
  })

  test("a primitive arm", () => {
    eq(
      __LOC__,
      scan([Text("a"), Text("continue"), Count(1.), Text("break"), Text("c")]),
      ["a", ".", "count", "."],
    )
    eq(__LOC__, scan([Count(1.), Count(-1.), Text("c")]), ["count", "."])
  })

  test("a class and an object arm", () => {
    eq(__LOC__, scan([Date(Date.make()), Text("a"), Object({x: 1}), Text("c")]), ["a", "."])
  })

  test("a default arm", () => {
    eq(
      __LOC__,
      scanDefault([Count(1.), Text("a"), Skip, Object({x: 1}), Date(Date.make()), Stop, Text("c")]),
      ["a", ".", "date", "."],
    )
    eq(__LOC__, scanDefaultBreak([Text("a"), Count(1.), Text("c")]), ["a"])
    eq(__LOC__, scanDefaultBreak([Text("a"), Object({x: 1}), Text("c")]), ["a"])
    eq(__LOC__, scanDefaultBreak([Date(Date.make()), Skip, Text("c")]), ["date"])
  })
})
