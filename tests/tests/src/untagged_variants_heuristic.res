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

// The string literals get a case, so `true` gets one too and the payload
// dispatch keeps its last arm unchecked
@unboxed
type withBool = | @as("a") LitA | @as("b") LitB | @as(true) LitTrue | Str(string) | Obj({x: int})

let caseBesideCases = (x: withBool) =>
  switch x {
  | Str(_) => 1
  | Obj(_) => 2
  | _ => 3
  }

// Likewise `null` gets a case instead of a null check on the object arm
@unboxed
type withNull = | @as("a") NullA | @as(null) NullLit | NullStr(string) | NullObj({x: int})

let nullBesideCases = (x: withNull) =>
  switch x {
  | NullStr(_) => 1
  | NullObj(_) => 2
  | _ => 3
  }
