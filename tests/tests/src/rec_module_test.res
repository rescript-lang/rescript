open Mocha
open Test_utils

module rec A: {
  let even: int => bool
} = {
  let even = n =>
    if n == 0 {
      true
    } else if n == 1 {
      false
    } else {
      B.odd(n - 1)
    }
}
and B: {
  let odd: int => bool
} = {
  let odd = n =>
    if n == 1 {
      true
    } else if n == 0 {
      false
    } else {
      A.even(n - 1)
    }
}

module rec AA: {
  let even: int => bool
  let x: unit => int
} = {
  let even = n =>
    if n == 0 {
      true
    } else if n == 1 {
      false
    } else {
      BB.odd(n - 1)
    }
  let x = () => BB.y() + 3
}
and BB: {
  let odd: int => bool
  let y: unit => int
} = {
  let odd = n =>
    if n == 1 {
      true
    } else if n == 0 {
      false
    } else {
      AA.even(n - 1)
    }
  let y = () => 32
}

module rec Even: {
  type t = Zero | Succ(Odd.t)
} = {
  type t = Zero | Succ(Odd.t)
}
and Odd: {
  type t = Succ(Even.t)
} = {
  type t = Succ(Even.t)
}

describe(__MODULE__, () => {
  test("test1", () => {
    eq(__LOC__, (true, true, false, false), (A.even(2), AA.even(4), B.odd(2), BB.odd(4)))
  })

  test("test2", () => {
    eq(__LOC__, BB.y(), 32)
  })

  test("test3", () => {
    eq(__LOC__, AA.x(), 35)
  })

  test("test4 - A.even", () => {
    eq(__LOC__, true, A.even(2))
  })

  test("test4 - AA.even", () => {
    eq(__LOC__, true, AA.even(4))
  })

  test("test5", () => {
    eq(__LOC__, false, B.odd(2))
  })
})
