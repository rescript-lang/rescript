// Char and int patterns share one column when checking exhaustiveness.
let f = x =>
  switch x {
  | 'a' => 1
  | 98 => 2
  }
