// A polymorphic variant payload is its name at run time, so #Foo is always
// matched as F, and #1 as One.
@unboxed type t = | @as("Foo") F | @as(1) One | P([#Foo | #Bar | #1])

let f = x =>
  switch x {
  | F => 1
  | One => 2
  | P(#Foo) => 3
  | P(#1) => 4
  | P(_) => 5
  }
