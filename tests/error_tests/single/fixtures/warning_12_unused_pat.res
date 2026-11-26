let describe = x =>
  switch x {
  | Some(0 | 0) => "zero"
  | _ => "other"
  }

let _ = describe(Some(1))
