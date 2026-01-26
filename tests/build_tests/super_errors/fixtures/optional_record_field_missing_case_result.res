type t = {b?: result<string, string>}

let f = v =>
  switch v {
  | {b: Ok(x)} => x
  | {b: Error(y)} => y
  }
