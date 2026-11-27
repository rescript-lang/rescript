let f = x =>
  switch x {
  | "abcd" => 0
  | "bcde" => 1
  | _ => assert(false)
  }
