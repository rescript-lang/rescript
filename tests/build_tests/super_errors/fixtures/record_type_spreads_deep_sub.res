// Checks that deep subsitution works as intended
type t<'a, 'b> = {x: result<'a, 'b>}
type d = {
  ...t<int, int>,
}

let d: d = {
  x: Ok("this errors"),
}
