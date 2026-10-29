// "a" is always matched as A, so S("a") can never match, wherever it appears.
@unboxed type t = | @as("a") A | S(string)

let afterLiteral = x =>
  switch x {
  | A => 1
  | S("a") => 2
  | S(_) => 3
  }

let beforeLiteral = x =>
  switch x {
  | S("a") => 2
  | A => 1
  | S(_) => 3
  }

let withoutLiteralCase = x =>
  switch x {
  | S("a") => 2
  | S(_) | A => 3
  }

let nested = x =>
  switch x {
  | (S("a"), _) => 1
  | (S(_) | A, _) => 2
  }

// Payloads that are not literal values still match.
let fine = x =>
  switch x {
  | S("b") => 1
  | S(s) => String.length(s)
  | A => 0
  }

// Every alternative can never match.
@unboxed type u = | @as("a") UA | @as("b") UB | US(string)

let allAlternatives = x =>
  switch x {
  | US("a") | US("b") => 1
  | US(_) | UA | UB => 2
  }
