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

let suites = {
  open Mt
  list{
    ("test1", _ => Eq((true, true, false, false), (A.even(2), AA.even(4), B.odd(2), BB.odd(4)))),
    ("test2", _ => Eq(BB.y(), 32)),
    ("test3", _ => Eq(AA.x(), 35)),
    ("test4", _ => Eq(true, A.even(2))),
    ("test4", _ => Eq(true, AA.even(4))),
    ("test5", _ => Eq(false, B.odd(2))),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
