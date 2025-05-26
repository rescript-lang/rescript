module String = Ocaml_String

let v = "gso"

let is_equal = () => {
  assert(String.get(v, 0) == 'g')
}

let is_exception = () =>
  try throw(Not_found) catch {
  | Not_found => ()
  }

let is_normal_exception = _x => {
  module E = {
    exception A(int)
  }
  let v = E.A(3)
  try throw(v) catch {
  | E.A(3) => ()
  }
}

let is_arbitrary_exception = () => {
  module E = {
    exception A
  }
  try throw(E.A) catch {
  | _ => ()
  }
}

let suites = list{
  ("is_equal", is_equal),
  ("is_exception", is_exception),
  ("is_normal_exception", is_normal_exception),
  ("is_arbitrary_exception", is_arbitrary_exception),
}

let e = Not_found
let eq = x =>
  switch x {
  | Not_found => true
  | _ => false
  }
exception Not_found
assert((e == Not_found) == false)
assert(eq(Not_found) == false)

Mt.from_suites("exception", suites)
