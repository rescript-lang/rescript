let f = () => throw(Not_found)

let assert_f = x => {
  let () = assert(x > 3)
  3
}

let hh = () => {
  let v = throw(Not_found)
  v + 3
}
/* TODO: comment for line column number */

exception A(int)
exception B
exception C(int, int)
let u = A(3)
/** note in ocaml, if such exception is not caught,
  it will be re-raised */
let test_not_found = (f, ()) =>
  try f() catch {
  | Not_found => 2
  }

let test_js_error2 = () =>
  try Js.Json.parseExn(` {"x" : }`) catch {
  | Js.Exn.Error(err) as e =>
    Js.log(Js.Exn.stack(err))
    throw(e)
  }

let test_js_error3 = () =>
  try {
    ignore(Js.Json.parseExn(` {"x"}`))
    1
  } catch {
  | e => 0
  }
