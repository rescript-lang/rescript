let f = x =>
  switch x {
  | Some(n) | None => n
  | _ => 0
  }

let _ = f(Some(1))
