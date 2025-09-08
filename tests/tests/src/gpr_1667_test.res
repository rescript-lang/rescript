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

{
  let i = {
    let e =
      {
        let o = false

        (
          z =>
            {
              let m = ()
              n => 0
            }((q, y) => o)
        )(o)
      } == 0
    0
  }
  eq(__LOC__, i, 0)
}

Mt.from_pair_suites(__MODULE__, suites.contents)
