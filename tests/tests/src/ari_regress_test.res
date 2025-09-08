let f = x => \"+"(x, ...)
let g = f(3)(4)

let h = ref(0)

let gg = (x, y) => {
  let u = x + y
  z => u + z
}

let g1 = (x, y) => {
  let u = x + y
  let () = incr(h)
  (xx, yy) => xx + yy + u
}
let x = gg(3, 5)(6)

let v = g1(3, 4)(6, _)

let suites = {
  open Mt
  list{
    ("curry", _ => Eq(g, 7)),
    (
      "curry2",
      _ => Eq(
        14,
        {
          ignore(v(1))
          v(1)
        },
      ),
    ),
    ("curry3", _ => Eq(x, 14)),
    (__LOC__, _ => Eq(h.contents, 2)),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
