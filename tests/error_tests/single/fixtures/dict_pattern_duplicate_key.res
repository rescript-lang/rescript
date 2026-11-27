let f = (d: dict<int>) =>
  switch d {
  | dict{"a": 1, "a": 2} => true
  | _ => false
  }
