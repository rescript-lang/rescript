@@config({
  flags: [],
})

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

let f: _ => string = %raw(` (a) => typeof a  `)

let a = f(3)
let b = f("3")

eq(__LOC__, a, "number")
eq(__LOC__, b, "string")
Mt.from_pair_suites(__FILE__, suites.contents)
