// Attributes before an aliased pattern belong to the alias
let f = x =>
  switch x {
  | @a y as z => 1
  | @a @b {y} as z => 2
  | @a (@b y) as z => 3
  | (@b y) as z => 4
  | Some(@a y as z) => 5
  | _ => 6
  }

// Constant patterns keep their attributes
let g = x =>
  switch x {
  | @a 1 => 1
  | @a "s" => 2
  | @a true => 3
  | @a 'a' .. 'z' => 4
  | Some(@a ()) => 5
  | @a 2 as z => z
  | _ => 6
  }
