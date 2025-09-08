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
  let number = 1_000_00
  let s = ref(String_set.empty)
  for i in 0 to number - 1 {
    s := String_set.add(Js.Int.toString(i), s.contents)
  }
  eq(__LOC__, String_set.cardinal(s.contents), number)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
