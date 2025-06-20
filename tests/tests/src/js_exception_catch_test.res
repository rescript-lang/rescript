let suites: ref<Mt.pair_suites> = ref(list{})

let add_test = {
  let counter = ref(0)
  (loc, test) => {
    incr(counter)
    let id = loc ++ (" id " ++ Js.Int.toString(counter.contents))
    suites := list{(id, test), ...suites.contents}
  }
}

let eq = (loc, x, y) => add_test(loc, _ => Mt.Eq(x, y))
let false_ = loc => add_test(loc, _ => Mt.Ok(false))

let true_ = loc => add_test(loc, _ => Mt.Ok(true))

let () = switch Js.Json.parseExn(` {"x"}`) {
| exception Js.Exn.Error(x) => true_(__LOC__)
| e => false_(__LOC__)
}

exception A(int)
exception B
exception C(int, int)

let test = f =>
  try {
    f()
    #No_error
  } catch {
  | Not_found => #Not_found
  | Invalid_argument("x") => #Invalid_argument
  | Invalid_argument(_) => #Invalid_any
  | A(2) => #A2
  | A(_) => #A_any
  | B => #B
  | C(1, 2) => #C
  | C(_) => #C_any
  | Js.Exn.Error(_) => #Js_error
  | e => #Any
  }

let () = {
  eq(__LOC__, test(_ => ()), #No_error)
  eq(__LOC__, test(_ => throw(Not_found)), #Not_found)
  eq(__LOC__, test(_ => invalid_arg("x")), #Invalid_argument)
  eq(__LOC__, test(_ => invalid_arg("")), #Invalid_any)
  eq(__LOC__, test(_ => throw(A(2))), #A2)
  eq(__LOC__, test(_ => throw(A(3))), #A_any)
  eq(__LOC__, test(_ => throw(B)), #B)
  eq(__LOC__, test(_ => throw(C(1, 2))), #C)
  eq(__LOC__, test(_ => throw(C(0, 2))), #C_any)
  eq(__LOC__, test(_ => Js.Exn.raiseError("x")), #Js_error)
  eq(__LOC__, test(_ => failwith("x")), #Any)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
