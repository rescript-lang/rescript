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

type t
@new external makeDate: unit => t = "Date"

let f = () => {
  let x = makeDate()
  let y = makeDate()
  (y > x, y < x, true)
}
/* y > x */
let (a0, a1, a2) = f()
Js.log2(a0, a1)
eq(__LOC__, a2, true)
Mt.from_pair_suites(__MODULE__, suites.contents)
