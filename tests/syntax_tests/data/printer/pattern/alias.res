let x as y = 1
let (x as y) as z = 1
let (Foo | Bar) as x = 1

// Attributes before the pattern belong to the alias; the aliased pattern's own
// attributes need parens
let f = x =>
  switch x {
  | @a y as z => 1
  | @a (@b y) as z => 2
  | (@b y) as z => 3
  | @a 1 => 4
  | Some(@a ()) => 5
  | _ => 6
  }
let g = (@a _) => 1
let h = (@a ()) => 1
