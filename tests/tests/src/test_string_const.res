module String = Ocaml_String

let f = String.get("ghsogh", 3)

let hh = try String.get("ghsogh", -3) catch {
| Invalid_argument(e) =>
  Js.log(e)
  'a'
}
