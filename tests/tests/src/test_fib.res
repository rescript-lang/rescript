let rec fib = x =>
  switch x {
  | 0
  | 1 => 1
  | n => fib(n - 1) + fib(n - 2)
  }

let rec fib2 = x =>
  switch x {
  | 1
  | 2 => 1
  | n => fib2(n - 1) + fib2(n - 2)
  }

/* let n = List.length */

let b = fib

let sum = {
  let v = ref(0)
  for i in 0 to 10 {
    v := v.contents + i
  }
  v.contents
}

let sumdown = {
  let v = ref(0)
  for i in 10 downto 0 {
    v := v.contents + i
  }
  v.contents
}

type rec list =
  | Nil
  | Cons(int, list)

let cons = (x, y) => Cons(x, y)

/* let cons2 (x,y) = Cons(x,y) (\* this seems to need be fixed *\) */
let rec length = x =>
  switch x {
  | Nil => 0
  | Cons(_, y) => 1 + length(y)
  }

let rec map = (f, x) =>
  switch x {
  | Nil => Nil
  | Cons(x, y) => Cons(f(x), map(f, y))
  }

let f = x => {
  let v = ref(x)
  let sum = ref(0)
  while v.contents > 0 {
    sum := sum.contents + v.contents
    decr(v)
  }
  sum.contents
}

let fib3 = n => {
  let rec fib_help = (a, b, n) =>
    if n > 0 {
      fib_help(b, a + b, n - 1)
    } else {
      a
    }
  fib_help(0, 1, n)
}
