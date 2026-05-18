let f = pair =>
  switch pair {
  | (Some(n), _) | (_, Some(n)) when n > 0 => n
  | _ => 0
  }
