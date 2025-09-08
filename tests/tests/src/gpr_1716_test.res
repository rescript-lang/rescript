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

type rec a = {b: b}
and b = {a: a}

let rec a = {b: b} and b = {a: a}

let is_inifite = (x: a) => x.b.a === x

eq(__LOC__, true, is_inifite(a))

Mt.from_pair_suites(__MODULE__, suites.contents)
