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

let f = x => {
  for i in 0 to 100 {
    Js.log(".") /* prevent optimization */
  }
  -x
}

let min_32_int = -2147483648
let u = f(min_32_int)

let () = {
  eq(__LOC__, min_32_int, u)
}
let () = Mt.from_pair_suites(__MODULE__, suites.contents)
