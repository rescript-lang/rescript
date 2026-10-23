let f = d =>
  switch d {
  | dict{} => 0
  | dict{"a": 1} => 1
  | dict{"a": ?Some(1), "b": ?None} => 2
  | dict{"a": dict{"b": x}} => x
  | @attr dict{"c": 3} => 3
  | dict{"d": 4} | dict{"e": 5} => 4
  | _ => 5
  }

let dict{"a": ?a} = someDict
