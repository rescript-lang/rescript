let unsafelyUnwrapOption = x =>
  switch x {
  | Some(v) => v
  | None => throw(Invalid_argument("Passed `None` to unsafelyUnwrapOption"))
  }
