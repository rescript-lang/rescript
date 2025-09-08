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

let f = h => h["x"]["y"]["z"]

let f2 = h => h["x"]["y"]["z"]

let f3 = (h, x, y) => h["paint"](x, y)["draw"](x, y)

let f4 = (h, x, y) => {
  h["paint"] = (x, y)
  h["paint"]["draw"] = (x, y)
}

/* let g h = */
/* h##(draw (x,y)) */
/* ##(draw (x,y)) */
/* ##(draw(x,y)) */
let () = eq(__LOC__, 32, f2({"x": {"y": {"z": 32}}}))

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
