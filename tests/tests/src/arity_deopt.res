let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

/* let f = fun x y */

/* let [@bs] f x y = .. */

/* let! f x y = */
/* let! x = f x y in */
/* let! y = f x y in */
/* let! z = f x y in */
/* return (x + y + z ) */

/* let f x y = */
/* let@bs z = f x y in */
/* let@b y = f x y in */
/* let@z z = f x y in */
/* return (x + y + z ) */

let f0 = (x, y, z) => x + y + z
/* catch up. In OCaml we can not tell the difference from below
   {[
     let f = fun [@bs] x y z -> x + y + z 
   ]}
*/
let f1 = x => (y, z) => x + y + z

let f2 = (x, y) => {
  let a = x
  z => a + y + z
}

let f3 = x => {
  let a = x
  (y, z) => a + y + z
}
/* be careful! When you start optimize functions of [@bs], its call site invariant 
   (Ml_app) will not hold any more.
   So the best is never shrink functons which could change arity
*/
let () = {
  eq(__LOC__, 6, ...)(f0(1, 2, 3))
  eq(__LOC__, 6, ...)(f1(1)(2, 3))
  eq(__LOC__, 6, ...)(f2(1, 2)(3))
  eq(__LOC__, 6, ...)(f3(1)(2, 3))
}
let () = Mt.from_pair_suites(__MODULE__, suites.contents)
