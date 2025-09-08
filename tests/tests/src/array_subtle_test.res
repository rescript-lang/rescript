module Array = Ocaml_Array

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, (x, y)) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

let v = [1, 2, 3, 3]

let () = eq(__LOC__, (4, Array.length(v)))

let () = {
  eq(__LOC__, (5, Js.Array2.push(v, 3))) /* in Js array length can be changing .. */
  eq(__LOC__, (5, Array.length(v)))
  eq(__LOC__, (5, Js.Array2.length(v)))
}

let () = {
  eq(__LOC__, (3, v[2]))
  v[2] = 4
  eq(__LOC__, (4, v[2]))
} /* should not inline */

let () = {
  while Js.Array2.length(v) > 0 {
    ignore(Js.Array2.pop(v))
  }
  eq(__LOC__, (0, Js.Array2.length(v)))
}

let f = v => {
  switch Js.Array2.pop(v) {
  | Some(x) => Js.log("hi")
  | None => Js.log("hi2")
  }
  Js.log(ignore(Js.Array2.pop(v)))
}

let fff = x => Array.length(x) >= 0

let fff2 = x =>
  if Array.length(x) >= 10 {
    Js.log("hi")
  }

let fff3 = x =>
  if Array.length(x) >= 0 {
    1
  } else {
    2
  }

let fff4 = x =>
  if Array.length(x) > 0 {
    1
  } else {
    2
  }

eq(__LOC__, (fff3([]), 1))
eq(__LOC__, (fff4([]), 2))
eq(__LOC__, (fff4([1]), 1))
let () = Mt.from_pair_suites(__MODULE__, suites.contents)
