let f = () =>
  for _ in 0 to 3 {
    try {
      ()
    } catch {
    | _ => ()
    } finally {
      continue
    }
  }
