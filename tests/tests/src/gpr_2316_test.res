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

let y = switch failwith("boo") {
| exception Failure(msg) => Some(msg)
| e => None
}

let x = switch failwith("boo") {
| exception Failure(msg) => Some(msg)
| e =>
  Js.log("ok")
  None
}

let () = {
  eq(__LOC__, y, Some("boo"))
  eq(__LOC__, x, Some("boo"))
}

Mt.from_pair_suites(__MODULE__, suites.contents)
