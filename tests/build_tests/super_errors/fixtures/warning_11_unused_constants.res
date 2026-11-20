let int = x =>
  switch x {
  | 5 .. 5 => 0
  | 5 => 1
  | 1 .. 3 => 2
  | 2 => 3
  | 4 | 7 => 4
  | 7 => 5
  | _ => 6
  }

let string = x =>
  switch x {
  | "a" => 0
  | "b" | "a" => 1
  | "b" => 2
  | _ => 3
  }

let variant = (x: [#A | #B(int) | #C]) =>
  switch x {
  | #A => 0
  | #B(1) => 1
  | #B(_) => 2
  | #B(1) => 3
  | #A | #C => 4
  }
