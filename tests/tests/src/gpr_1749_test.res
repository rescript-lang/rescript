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

let a = if 1. < (1. < 1. ? 1. : 10.) {
  0
} else {
  1
}

eq(__LOC__, 0, a)
Mt.from_pair_suites(__MODULE__, suites.contents)
