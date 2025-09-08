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

let () = {
  eq(__LOC__, max_int, %raw("2147483647"))
  eq(__LOC__, min_int, %raw("-2147483648"))
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
