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

let check_healty = check => !check["a"] && (!check["b"] && !check["c"])

let basic_not = x => !x

let f = check => check["x"] && check["y"]
/* [x && y] in OCaml can be translated to [x && y] in JS */

let () = eq(__LOC__, f({"x": true, "y": false}), false)

let () = eq(__LOC__, check_healty({"a": false, "b": false, "c": true}), false)

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
