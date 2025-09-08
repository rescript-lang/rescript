open Belt

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

module N = List

module V = Ext_pervasives_test.LargeFile

module J = Js.Json

module type X = module type of List

let f = x => {
  module L = List
  Js.log(x)
  Js.log(List.length(x))
  module(L: X)
}

let a = {
  let h = f(list{})
  let module(L: X) = h
  L.length(list{1, 2, 3})
}

eq(__LOC__, a, 3)
Mt.from_pair_suites(__MODULE__, suites.contents)
