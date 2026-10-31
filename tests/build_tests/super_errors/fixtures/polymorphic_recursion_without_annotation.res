type rec nested<'a> = Flat('a) | Nest(nested<array<'a>>)

let rec depth = n =>
  switch n {
  | Flat(_) => 0
  | Nest(n) => 1 + depth(n)
  }
