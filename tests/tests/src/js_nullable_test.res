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

type element
type dom
@send @return(nullable) external getElementById: (dom, string) => option<element> = "getElementById"

let test = dom => {
  let elem = dom->getElementById("haha")
  switch elem {
  | None => 1
  | Some(ui) =>
    Js.log(ui)
    2
  }
}

let f = (x, y) => {
  Js.log("no inline")
  Js.Nullable.return(x + y)
}

eq(__LOC__, Js.isNullable(Js.Nullable.return(3)), false)

eq(__LOC__, Js.isNullable(f(1, 2)), false)

eq(__LOC__, Js.isNullable(%raw("null")), true)

{
  let null2 = Js.Nullable.return(3)
  let null = null2
  eq(__LOC__, Js.isNullable(null), false)
}
Mt.from_pair_suites(__MODULE__, suites.contents)
