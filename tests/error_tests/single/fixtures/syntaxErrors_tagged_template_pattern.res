let classify = value =>
  switch value {
  | json`\x61` => 1
  | _ => 2
  }
