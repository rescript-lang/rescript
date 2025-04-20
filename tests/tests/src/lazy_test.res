let u = ref(3)
let v = Lazy.make(() => u := 32)

let lazy_test = () => {
  let h = u.contents
  let g = {
    Lazy.get(v)
    u.contents
  }
  (h, g)
}

/* lazy_match isn't available anymore */
// let f = x =>
//   switch x {
//   | (lazy (), _, {contents: None}) => 0
//   | (_, lazy (), {contents: Some(x)}) => 1
//   }

// /* PR #5992 */
// /* Was segfaulting */
// let s = ref(None)
// let set_true = lazy (s := Some(1))
// let set_false = lazy (s := None)

let u_v = ref(0)
let u = Lazy.make(() => u_v := 2)
let () = Lazy.get(u)

/* module Mt = Mock_mt */

let exotic = x =>
  switch x {
  /* Lazy in a pattern. (used in advi) */
  | y => Lazy.get(y)
  }

/* let l_from_val = Lazy.from_val 3 */

let l_from_fun = Lazy.make(_ => 3)
let forward_test = Lazy.make(() => {
  let u = ref(3)
  incr(u)
  u.contents
})
/* module Mt = Mock_mt */

let f005 = Lazy.make(() => 1 + 2 + 3)

let f006: Lazy.t<unit => int> = Lazy.make(() => {
  let x = 3
  _ => x
})

let f007 = Lazy.make(() => raise(Not_found))
let f008 = Lazy.make(() => {
  Js.log("hi")
  raise(Not_found)
})

let a2 = x => Lazy.from_val(x)

let a3 = Lazy.from_val(3)
let a4 = a2(3)
let a5 = Lazy.from_val(None)
let a6 = Lazy.from_val()

let a7 = Lazy.get(a5)
let a8 = Lazy.get(a6)

Mt.from_pair_suites(
  __MODULE__,
  {
    open Mt
    list{
      ("simple", _ => Eq(lazy_test(), (3, 32))),
      // ("lazy_match", _ => Eq(h, 2)),
      ("lazy_force", _ => Eq(u_v.contents, 2)),
      ("lazy_from_fun", _ => Eq(Lazy.get(l_from_fun), 3)),
      ("lazy_from_val", _ => Eq(Lazy.get(Lazy.from_val(3)), 3)),
      ("lazy_from_val2", _ => Eq(Lazy.get(Lazy.get(Lazy.from_val(Lazy.make(() => 3)))), 3)),
      (
        "lazy_from_val3",
        _ => Eq(
          {
            %debugger
            Lazy.get(Lazy.get(Lazy.make(() => forward_test)))
          },
          4,
        ),
      ),
      (__FILE__, _ => Eq(a3, a4)),
      (__FILE__, _ => Eq(a7, None)),
      (__FILE__, _ => Eq(a8, ())),
      (__LOC__, _ => Ok(Lazy.isEvaluated(Lazy.from_val(3)))),
      (__LOC__, _ => Ok(!Lazy.isEvaluated(Lazy.make(() => raise(Not_found))))),
    }
  },
)
