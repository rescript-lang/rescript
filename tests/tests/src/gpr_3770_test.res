type t = Foo(int, int, int)

let show = x =>
  switch x {
  | Foo(0, 0, 0) => "zeroes"
  | Foo(a, b, _) => Int.toString(a) ++ Int.toString(b)
  }
