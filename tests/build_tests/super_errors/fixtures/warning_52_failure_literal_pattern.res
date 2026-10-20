let describe = e =>
  switch e {
  | Failure("boom") => "boom"
  | _ => "other"
  }
