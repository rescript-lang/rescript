let ff = v => Js.Float.toString(v)
let f = v => Js.Int.toString(v)

Mt.from_pair_suites(
  __MODULE__,
  list{
    (__LOC__, _ => Eq(ff(infinity), "Infinity")),
    (__LOC__, _ => Eq(ff(neg_infinity), "-Infinity")),
  },
)
