let re2 = Js.Re.fromStringWithFlags("foo", ~flags="gi")

let capture_access = switch re2->Js.Re.exec_("Foo") {
| None => 0
| Some(r) =>
  switch Js.Re.captures(r) {
  | [Value(full), _] => String.length(full)
  | _ => 0
  }
}
