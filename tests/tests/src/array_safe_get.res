module Array = Ocaml_Array

let x = [1, 2]
let y = try x[3] catch {
| Invalid_argument(msg) =>
  Js.log(msg)
  0
}
