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

let f = x =>
  switch x {
  | y => Lazy.get(y) ++ "abc"
  }

let u = {
  let x = Lazy.from_fun(() => "def")
  ignore(Lazy.get(x))
  f(x)
}

eq(__LOC__, u, "defabc")
Mt.from_pair_suites(__MODULE__, suites.contents)
