module M: {
  let v: int
} = {
  type t = ..
  type t += Foo
  let _describe = (x: t) =>
    switch x {
    | Foo => "foo"
    | _ => "other"
    }
  let v = 1
}
