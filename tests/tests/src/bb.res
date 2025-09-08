let f = x =>
  switch x {
  | #a => "a"
  | #b => "b"
  | #c => "c"
  }

let ff = x =>
  switch x {
  | "a" => #a
  | "b" => #b
  | "c" => #c
  | _ => assert(false)
  }

let test = x =>
  switch switch x {
  | "a" => #a
  | "b" => #b
  | "c" => #c
  | _ => assert(false)
  } {
  | #a => "a"
  | #b => "b"
  | #c => "c"
  }

let test_poly = switch #a {
| #a => "a"
| #b => "b"
| #c => "c"
}

let (c, d, e) = (f(#a), f(#b), f(#c))
