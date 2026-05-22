let f = pair =>
  switch pair {
  | (Some(a), Some(b), _) | (_, Some(a), Some(b)) if a + b > 0 => a + b
  | _ => 0
  }
