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

@send external map: (Js_array2.t<'a>, 'a => 'b) => Js_array2.t<'b> = "map"

let () = eq(__LOC__, map([1, 2, 3, 4], x => x + 1), [2, 3, 4, 5])

Mt.from_pair_suites(__MODULE__, suites.contents)
