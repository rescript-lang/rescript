type t = (~x: int, ~y: string) => option<int>

type u = (~x: int, ~y: string) => option<int>

let f = (x: t): u => x

let u: u = (~x, ~y) =>
  switch Int.fromString(y) {
  | Some(y) => Some(x + y)
  | None => None
  }

let u1 = (f: u) => {
  f(~y="x", ~x=2)->Console.log
  f(~x=2, ~y="x")->Console.log
}

let inferredOrder = g => (g(~a=1, ~b=2), g(~b=3, ~a=4))

let h = (~x: unit) => 3

let a = u1(u)

type u0 = (~x: int=?, ~y: string) => int
