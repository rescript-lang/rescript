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

let () = eq(
  __LOC__,
  "ghso ghso g"->Js.String2.split(" ")->Js.Array2.reduce((x, y) => x ++ ("-" ++ y), ""),
  "-ghso-ghso-g",
)

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
