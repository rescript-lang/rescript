open Mocha
open Test_utils

let called = ref(0)
/* function hoisting prevent the toplevel bug */
let g = () => {
  let rec v = ref(next)
  and next = (i, b) => {
    incr(called)
    if b {
      ignore(v.contents(i, false))
    }
    i + 1
  }

  Js.log(Js.Int.toString(next(0, true)))
}

g()

let rec x = list{1, ...y}
and y = list{2, ...x}

describe(__MODULE__, () => {
  test("called contents", () => eq(__LOC__, 2, called.contents))
})
