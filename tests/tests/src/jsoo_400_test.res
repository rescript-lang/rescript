let u = () =>
  switch String.length("123") {
  | n => 3 / 0
  | exception _ => 42
  } /* TODO: could be optimized */

Mt.from_pair_suites(__MODULE__, list{(__LOC__, _ => ThrowAny(_ => ignore(u())))})
