open Mocha
open Test_utils
open Js

test("js_exception_catch_test_json_parse", () => {
  switch Js.Json.parseExn(` {"x"}`) {
  | exception Js.Exn.Error(x) => ok(__LOC__, true)
  | e => ok(__LOC__, false)
  }
})

exception A(int)
exception B
exception C(int, int)

let testException = f =>
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

describe(__MODULE__, () => {
  test("js exception catch test", () => {
    eq(__LOC__, testException(_ => ()), #No_error)
    eq(__LOC__, testException(_ => throw(Not_found)), #Not_found)
    eq(__LOC__, testException(_ => invalid_arg("x")), #Invalid_argument)
    eq(__LOC__, testException(_ => invalid_arg("")), #Invalid_any)
    eq(__LOC__, testException(_ => throw(A(2))), #A2)
    eq(__LOC__, testException(_ => throw(A(3))), #A_any)
    eq(__LOC__, testException(_ => throw(B)), #B)
    eq(__LOC__, testException(_ => throw(C(1, 2))), #C)
    eq(__LOC__, testException(_ => throw(C(0, 2))), #C_any)
    eq(__LOC__, testException(_ => Js.Exn.raiseError("x")), #Js_error)
    eq(__LOC__, testException(_ => failwith("x")), #Any)
  })
})
