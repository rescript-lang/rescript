let f = x =>
  switch x {
  | when when when > 0 => when
  | as as as => as
  | exception => exception
  | exception when ready => 1
  | exception as e => e
  | exception (as) => 2
  | exception (of) => 3
  | exception exception => 4
  | exception Not_found => 5
  | {open, private: p} => 6
  | (rec, of) => 7
  | _ => 0
  }

let {private, as} = options
let (rec as x) = 1
let f = (~as as as) => as
let f = (~open as open=false) => open
