type inner =
  | P(int)
  | S(int)

type outer =
  | A(inner)
  | B(inner)
  | C(inner)

let f = x =>
  /* These cause unreachable code */
  switch x {
  | A(P(a)) => a + a
  | A(S(a)) => a - a
  /* These don't, because there's commonality between them */
  | B(P(a)) | B(S(a)) | C(P(a)) | C(S(a)) => a * a
  }

let ff = c =>
  switch {
    let a = 1
    let b = 1
    incr(c)
    a + c.contents + b
  } {
  | 0 => 1
  | 1 => 2
  | 2 => 3
  | 3 => 4
  | _ => 0
  }
