// true is always matched as Yes, so Ans(true) can never match, and a match
// covering Yes and Ans(false) is exhaustive (no warning for `exhaustive`).
@unboxed type b = | @as(true) Yes | Ans(bool)

let exhaustive = x =>
  switch x {
  | Yes => 1
  | Ans(false) => 2
  }

let deadArm = x =>
  switch x {
  | Yes => 1
  | Ans(true) => 3
  | Ans(false) => 2
  }
