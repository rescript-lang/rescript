let f = (x: (option<int>, option<int>)) =>
  switch x {
  | (Some(a), None) | (None, Some(a)) => a
  | (Some(a), Some(a)) => a
  | (None, None) => 0
  }
