let f = pair =>
  switch pair {
  | (Some(n), _) | (_, Some(n)) if n > 0 => n
  | _ => 0
  }
