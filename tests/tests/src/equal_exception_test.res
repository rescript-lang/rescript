open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("exception", () => {
    try throw(Not_found) catch {
    | Not_found => ()
    }
  })

  test(" is_normal_exception", () => {
    module E = {
      exception A(int)
    }
    let v = E.A(3)
    try throw(v) catch {
    | E.A(3) => ()
    }
  })

  test("is_arbitrary_exception", () => {
    module E = {
      exception A
    }
    try throw(E.A) catch {
    | _ => ()
    }
  })
})

let e = Not_found
let eq = x =>
  switch x {
  | Not_found => true
  | _ => false
  }
exception Not_found
assert((e == Not_found) == false)
assert(eq(Not_found) == false)
