let f = (x: int) =>
  switch x {
  | 1 | "hello" => "weird"
  | _ => "other"
  }

let _ = f(1)
