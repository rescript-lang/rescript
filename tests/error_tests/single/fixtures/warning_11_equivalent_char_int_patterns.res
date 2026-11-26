// `char` is an alias of `int`, so `97` repeats `'a'`.
let f = x =>
  switch x {
  | 'a' => 1
  | 97 => 2
  | _ => 3
  }
