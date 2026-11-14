let id = x => x

let konst = (x, _y) => x

let second = (_x: 'a, y: 'b): 'b => y

let pair = (a, b) => (a, b)

let applyTwice = (f, x) => f(f(x))

let getOr = (o, d) =>
  switch o {
  | Some(v) => v
  | None => d
  }

let useId = (n: int) => id(n) + applyTwice(x => x + 1, n)
