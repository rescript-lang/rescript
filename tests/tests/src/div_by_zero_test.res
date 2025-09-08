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
let add = suite => suites := list{suite, ...suites.contents}

let () = {
  add((__LOC__, _ => ThrowAny(_ => ignore(3 / 0))))
  add((__LOC__, _ => ThrowAny(_ => ignore(mod(3, 0)))))
}

let div = (x, y) => x / y + 3

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
