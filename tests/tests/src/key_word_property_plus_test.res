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
  Js.Array2.reduce([1, 2, 3, 4], \"+", 0),
  {
    open Ident_mangles
    __dirname + __filename + exports + require
  },
)

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
