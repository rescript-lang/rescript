module N = {
  let add = (x, y) => x + y
}

module type S0 = {
  let f1: unit => unit
  let f2: (unit, unit) => unit
  let f3: (unit, unit, unit) => unit
}

module N0: S0 = {
  let f4 = (_, _, _) => ()
  let f1 = _ => ()
  let f2 = (_, _) => ()
  let f3 = (_, _, _) => ()
}

module N1 = {
  let f4 = (_, _, _) => ()
  let f1 = _ => ()
  let f2 = (_, _) => ()
  let f3 = (_, _, _) => ()
}
