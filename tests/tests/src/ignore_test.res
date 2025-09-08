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

let f = x => ignore(x)

let ff = x => ignore(Js.log(x))

let () = eq(__LOC__, f(3), ())

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
