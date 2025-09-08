/* [@@@warning "a-11-8-4-a"] */
@@warning("a+11+8-8")
type t6 =
  | T60
  | T61
  | T62
  | T63
  | T64(int)
  | T65(int)
  | T66(int)
  | T68(int)

let f9 = x =>
  switch x {
  | T60
  | T61
  | T62 => 1
  | T64(_)
  | T65(_) => 2
  | _ => 3
  }
