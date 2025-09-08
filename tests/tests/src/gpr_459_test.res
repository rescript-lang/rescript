@@config({no_export: no_export})

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

let uu = {
  "_'x": 3,
}

let uu2 = {
  "_then": 1,
  "catch": 2,
  "_'x": 3,
}

let hh = uu["_'x"]

let () = eq(__LOC__, hh, 3)

let () = eq(__LOC__, (1, 2, 3), (uu2["_then"], uu2["catch"], uu2["_'x"]))

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
