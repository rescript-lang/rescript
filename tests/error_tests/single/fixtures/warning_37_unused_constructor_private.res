module M: {
  type t = private Foo | Bar
  let v: int
} = {
  type t = Foo | Bar
  let v = 1
}
