let f = (d: dict<int>) =>
  switch d {
  | {foo: 1} => true
  | _ => false
  }
