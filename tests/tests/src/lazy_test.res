open Mocha
open Test_utils

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

let f005 = Lazy.make(() => 1 + 2 + 3)

let f006: Lazy.t<unit => int> = Lazy.make(() => {
  let x = 3
  _ => x
})

let f007 = Lazy.make(() => throw(Not_found))
let f008 = Lazy.make(() => {
  Js.log("hi")
  throw(Not_found)
})

let a2 = x => Lazy.from_val(x)

let a3 = Lazy.from_val(3)
let a4 = a2(3)
let a5 = Lazy.from_val(None)
let a6 = Lazy.from_val()

let a7 = Lazy.get(a5)
let a8 = Lazy.get(a6)

describe(__MODULE__, () => {
  test("simple", () => eq(__LOC__, lazy_test(), (3, 32)))
  // test("lazy_match", () => eq(__LOC__, h, 2))
  test("lazy_force", () => eq(__LOC__, u_v.contents, 2))
  test("lazy_from_fun", () => eq(__LOC__, Lazy.get(l_from_fun), 3))
  test("lazy_from_val", () => eq(__LOC__, Lazy.get(Lazy.from_val(3)), 3))
  test("lazy_from_val2", () =>
    eq(__LOC__, Lazy.get(Lazy.get(Lazy.from_val(Lazy.make(() => 3)))), 3)
  )
  test("lazy_from_val3", () =>
    eq(
      __LOC__,
      {
        %debugger
        Lazy.get(Lazy.get(Lazy.make(() => forward_test)))
      },
      4,
    )
  )
  test(__FILE__, () => eq(__LOC__, a3, a4))
  test(__FILE__, () => eq(__LOC__, a7, None))
  test(__FILE__, () => eq(__LOC__, a8, ()))
  test(__LOC__, () => ok(__LOC__, Lazy.isEvaluated(Lazy.from_val(3))))
  test(__LOC__, () => ok(__LOC__, !Lazy.isEvaluated(Lazy.make(() => throw(Not_found)))))
})
