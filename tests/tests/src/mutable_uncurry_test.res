@@config({
  flags: [
    /* "-bs-diagnose" */
  ],
})

open Mocha
open Test_utils

let eq2 = (x, {contents: y}) => x.contents == y

let ut3 = ({contents: x0}, {contents: x1}, {contents: x2}) => (x0, x1, x2)
let t3 = ({contents: x0}, {contents: x1}, {contents: x2}) => (x0, x1, x2)

let ut4 = ({contents: x0}, {contents: x1}, {contents: x2}, {contents: x3}) => (x0, x1, x2, x3)

let t4 = ({contents: x0}, {contents: x1}, {contents: x2}, {contents: x3}) => (x0, x1, x2, x3)

let ut5 = ({contents: x0}, {contents: x1}, {contents: x2}, {contents: x3}, {contents: x4}) => (
  x0,
  x1,
  x2,
  x3,
  x4,
)

let t5 = ({contents: x0}, {contents: x1}, {contents: x2}, {contents: x3}, {contents: x4}) => (
  x0,
  x1,
  x2,
  x3,
  x4,
)

let nested0 = ({contents: x0}, {contents: x1}, {contents: x2}) => {
  let a = x0 + x1 + x2
  ({contents: x0}, {contents: x1}, {contents: x2}) => a + x0 + x1 + x2
}

let nested1 = ({contents: x0}, {contents: x1}, {contents: x2}) => {
  let a = x0 + x1 + x2
  ({contents: x0}, {contents: x1}, {contents: x2}) => a + x0 + x1 + x2
}

describe(__MODULE__, () => {
  test("eq with different refs", () => eq(__LOC__, false, eq2(ref(1), ref(2))))
  test("eq with same refs", () => eq(__LOC__, true, eq2(ref(2), ref(2))))
  test("ut3 function", () => eq(__LOC__, ut3(ref(1), ref(2), ref(3)), (1, 2, 3)))
  test("t3 function", () => eq(__LOC__, t3(ref(1), ref(2), ref(3)), (1, 2, 3)))
  test("ut5 function", () =>
    eq(__LOC__, ut5(ref(1), ref(2), ref(3), ref(1), ref(1)), (1, 2, 3, 1, 1))
  )
})
