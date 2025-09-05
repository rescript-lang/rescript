let f = x =>
  switch x {
  | ("a" | _) as #...f => 1
  | _ => 0
  }
