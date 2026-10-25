@typeOf("boolean") type jsBool
@unboxed type t = | @as(true) Yes | Value(jsBool)
let f = (v: t) =>
  switch v {
  | Yes => 1
  }

type alias = bool
@unboxed type u = | @as(true) Yes2 | Value2(alias)
let g = (v: u) =>
  switch v {
  | Yes2 => 1
  | Value2(false) => 2
  }
