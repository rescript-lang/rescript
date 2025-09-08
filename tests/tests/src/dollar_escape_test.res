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

let \"$$" = (x, y) => x + y

let v = \"$$"(1, 2)

let \"$$+" = (x, y) => x * y

let u = \"$$+"(1, 3)

eq(__LOC__, v, 3)
eq(__LOC__, u, 3)
Mt.from_pair_suites(__MODULE__, suites.contents)
