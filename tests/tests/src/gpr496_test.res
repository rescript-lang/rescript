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

let expected = (
  true == false,
  false == true,
  false == false,
  true == true,
  Pervasives.compare(false, true),
  Pervasives.compare(true, false),
  Pervasives.compare(false, false),
  Pervasives.compare(true, true),
)

let expected2 = (false, false, true, true, -1, 1, 0, 0)
let u = (
  true == false,
  false == true,
  false == false,
  true == true,
  Pervasives.compare(false, true),
  Pervasives.compare(true, false),
  Pervasives.compare(false, false),
  Pervasives.compare(true, true),
)

let () = eq(__LOC__, expected, u)

let () = eq(__LOC__, expected, expected2)

let ff = (x: bool, y) => min(x, y())
let () = eq(__LOC__, min(true, false), false)

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
