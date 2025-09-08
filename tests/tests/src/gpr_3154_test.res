let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)
let b = (loc, x) => Mt.bool_suites(~test_id, ~suites, loc, x)

module J = Js.Dict

let () = {
  let d = Js.Dict.empty()
  J.set(d, "foo", None)
  switch J.get(d, "foo") {
  | Some(None) => b(__LOC__, true)
  | _ => b(__LOC__, false)
  }
}

let () = {
  let d0 = Js.Dict.empty()
  J.set(d0, "foo", None)
  eq(__LOC__, J.get(d0, "foo"), Some(None))
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
