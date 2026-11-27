type t = Foo | Bar(int)

let f = x =>
  switch x {
  | Foo(_) => 0
  | Bar(_) => 1
  }
