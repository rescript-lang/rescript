let bad: Gadt.t<int> => string = x =>
  switch x {
  | Int(n) => n
  | Pair(_, _) => "pair"
  }
