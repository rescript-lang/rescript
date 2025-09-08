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
  eq(__LOC__, Js.Null.empty, Js.Null.empty)
  switch Js.Types.classify(Js.Null.empty) {
  | JSNull => eq(__LOC__, true, true)
  | _ => eq(__LOC__, true, false)
  }
  eq(__LOC__, true, Js.Types.test(Js.Null.empty, Null))
}

Mt.from_pair_suites(__LOC__, suites.contents)
