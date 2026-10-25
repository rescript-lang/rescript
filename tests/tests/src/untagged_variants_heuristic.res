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
