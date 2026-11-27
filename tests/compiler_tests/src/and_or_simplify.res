@unboxed
type t = | @as(null) Null | @as(undefined) Undefined | B(bool) | S(string) | I(int)

let check_null_eq_typeof = x =>
  switch x {
  | B(_) if x == Null => 3
  | _ => 4
  }

let check_null_neq_typeof = x =>
  switch x {
  | B(_) if x != Null => 3
  | _ => 4
  }

let check_undefined_eq_typeof = x =>
  switch x {
  | B(_) if x == Undefined => 3
  | _ => 4
  }

let check_undefined_neq_typeof = x =>
  switch x {
  | B(_) if x != Undefined => 3
  | _ => 4
  }
