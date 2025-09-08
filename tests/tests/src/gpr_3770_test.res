type t = Foo(int, int, int)

let show = x =>
  switch x {
  | Foo(0, 0, 0) => "zeroes"
  | Foo(a, b, _) => Js.Int.toString(a) ++ Js.Int.toString(b)
  }
