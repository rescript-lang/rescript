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

let a0 = try {
  let _c = 0 / 0
  0
} catch {
| _ => 1
}

let a1 = try {
  let _h = mod(0, 0)
  0
} catch {
| _ => 1
}

eq(__LOC__, (a0, a1), (1, 1))

Mt.from_pair_suites(__MODULE__, suites.contents)
