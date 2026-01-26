let f = x => x + 1
let rec fib = x =>
  switch x {
  | 1 | 2 => 1
  | n => fib(n - 1) + fib(n - 2)
  }

type rec intlist =
  | Nil
  | Cons(int, intlist)

let nil = Nil
let cons = (x, y) => Cons(x, y)

let rec map = (f, x) =>
  switch x {
  | Nil => Nil
  | Cons(x, xs) => Cons(f(x), map(f, xs))
  }

let sum = n => {
  let v = ref(0)
  for i in 0 to n {
    v := v.contents + i
  }
  v.contents
}

let len = Belt.List.length

let f = (g, x) => {
  let u = switch g(x) {
  | "aabb" => 0
  | "bbc" => 1
  | _ => 2
  }
  u + 3
}

@@warning("-37-8")

let f = (g, x) => {
  let u = switch g(x) {
  | "aabb" => 0
  | "bbc" => 1
  }
  u + 3
}

let v = (a, ()): unit =>
  while {
    ignore(true)
    a(3)
  } {
    ignore(Console.log(3))
  }

type cxt =
  | A
  | B(int)
  | C(int, string)
  | D
  | E
  | F(string)

let f = (x: cxt) => {
  let u = switch x {
  | A => 0
  | B(_) => 1
  | C(_) => 2
  | D => 3
  | E => 4
  | F(_) => 5
  }
  u + 3
}

let f = (g, x: cxt) =>
  g(
    switch x {
    | A => 0
    | B(_) => 1
    | C(_) => 2
    | D => 3
    | E => 4
    | F(_) => 5
    },
  )

let f = (h, g, x) =>
  h(
    try g(x) catch {
    | Not_found => 0
    },
  )

let f = (x, y, z) => x + y + z
let g = (x, y) => {
  let u = x + y
  z => u + z
}

let g1 = (x, y) => {
  let u = x + y
  (xx, yy) => xx + yy + u
}
let x = g(3, 5)(6)

let v = g1(3, 4)(6, ...)
