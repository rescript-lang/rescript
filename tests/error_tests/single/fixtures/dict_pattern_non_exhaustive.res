// Counter-examples print as dict patterns
let f = (d: dict<int>) =>
  switch d {
  | dict{"a": 1} => 1
  | dict{"b": ?None} => 2
  }

type r = {x: dict<int>}

let g = r =>
  switch r {
  | {x: dict{"a": 1}} => 1
  }
