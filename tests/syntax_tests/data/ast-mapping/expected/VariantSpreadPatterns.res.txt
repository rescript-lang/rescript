let f = x =>
  switch x {
  | ...a => 1
  | ...a as v => v
  | @attr ...a => 2
  | Some(...a) => 3
  | ...M.a | ...b => 4
  | #...p => 5
  | @attr #...p => 6
  }
