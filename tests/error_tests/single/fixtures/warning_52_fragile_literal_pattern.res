let describe = e =>
  switch e {
  | Invalid_argument("Foo") => "specific"
  | _ => "other"
  }

let _ = describe(Invalid_argument("bar"))
