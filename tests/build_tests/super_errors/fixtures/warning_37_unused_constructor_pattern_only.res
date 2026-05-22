module M: {
  let v: int
} = {
  type t = Foo | Bar
  let _describe = (x: t) =>
    switch x {
    | Foo => "foo"
    | Bar => "bar"
    }
  let v = 1
}
