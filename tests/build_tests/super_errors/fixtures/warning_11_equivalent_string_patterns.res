let classify = value =>
  switch value {
  | "a" => 1
  | "\x61" => 2
  | _ => 3
  }
