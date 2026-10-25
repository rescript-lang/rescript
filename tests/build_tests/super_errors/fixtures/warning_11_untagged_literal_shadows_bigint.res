@unboxed type t = | @as(2n) Two | BI(bigint) | S(string)
let f = (v: t) =>
  switch v {
  | BI(02n) => "dead"
  | BI(_) => "bigint"
  | _ => "other"
  }
