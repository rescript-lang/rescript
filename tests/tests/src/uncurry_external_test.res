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

%%raw(`
function sum(a,b){
  return a + b
}
`)

external sum: (float, float) => float = "sum"

let h = sum(1.0, 2.0)

let () = eq(__LOC__, h, 3.)
let () = Mt.from_pair_suites(__MODULE__, suites.contents)
