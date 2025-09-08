@unboxed
type t<'a> =
  | Bool(bool)
  | @as(false) False
  | @as(true) True
