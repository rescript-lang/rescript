let f = (x: JSON.t) =>
  switch x {
  | Null => Console.log("abc")
  | _ => ()
  }

@unboxed
type t =
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | Int(int)

let ff = (x: t) =>
  switch x {
  | Int(_) => Console.log("abc")
  | _ => ()
  }

// The literal without an arm reaches the default through the case of its
// kind, before any class check, so it needs no case of its own
@unboxed
type withDate =
  | @as("a") WithA | WithText(string) | WithCount(float) | WithDate(Date.t) | WithObject({x: int})

let dateOrObject = (x: withDate) =>
  switch x {
  | WithDate(_) => 1
  | WithObject(_) => 2
  | _ => 3
  }

// Arms of different kinds with one body are one unchecked branch
@unboxed
type pointOrName = | @as(null) NoPoint | Point({x: int}) | Name(string)

let logPointOrName = (x: pointOrName) =>
  switch x {
  | NoPoint => Console.log("null")
  | Point(_) => Console.log("payload")
  | Name(_) => Console.log("payload")
  }
