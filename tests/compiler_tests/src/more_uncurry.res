let f = (x, y, x_) =>
  switch x_ {
  | None => x + y
  | Some(z) => x + y + z
  }
