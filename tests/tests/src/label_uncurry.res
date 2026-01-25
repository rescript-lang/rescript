type t = (~x: int, ~y: string) => option<int>

type u = (~x: int, ~y: string) => option<int>

let f = (x: t): u => x

let u: u = (~x, ~y) =>
  switch Belt.Int.fromString(y) {
  | Some(y) => Some(x + y)
  | None => None
  }

let u1 = (f: u) => {
  f(~y="x", ~x=2)->Console.log
  f(~x=2, ~y="x")->Console.log
}
let h = (~x: unit) => 3

let a = u1(u)

type u0 = (~x: int=?, ~y: string) => int
