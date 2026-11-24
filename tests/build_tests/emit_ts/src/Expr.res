type op = Add | Sub | Mul | Div

type rec expr =
  | Lit(int)
  | Var(string)
  | Neg(expr)
  | Bin(op, expr, expr)

let zero = Lit(0)

let rec unwrapNeg = (e: expr): expr =>
  switch e {
  | Neg(Neg(inner)) => unwrapNeg(inner)
  | _ => e
  }

let rec size = (e: expr): int =>
  switch e {
  | Lit(_) | Var(_) => 1
  | Neg(a) => 1 + size(a)
  | Bin(_, a, b) => 1 + size(a) + size(b)
  }

let rec depth = (e: expr): int =>
  switch e {
  | Lit(_) | Var(_) => 1
  | Neg(a) => 1 + depth(a)
  | Bin(_, a, b) =>
    let da = depth(a)
    let db = depth(b)
    1 + (da > db ? da : db)
  }

let rec simplify = (e: expr): expr =>
  switch e {
  | Lit(_) | Var(_) => e
  | Neg(a) =>
    switch simplify(a) {
    | Lit(n) => Lit(-n)
    | Neg(inner) => inner
    | sa => Neg(sa)
    }
  | Bin(op, a, b) =>
    switch (op, simplify(a), simplify(b)) {
    | (Add, Lit(x), Lit(y)) => Lit(x + y)
    | (Sub, Lit(x), Lit(y)) => Lit(x - y)
    | (Mul, Lit(x), Lit(y)) => Lit(x * y)
    | (Add, Lit(0), sb) => sb
    | (Add, sa, Lit(0)) => sa
    | (Mul, Lit(1), sb) => sb
    | (Mul, sa, Lit(1)) => sa
    | (Mul, Lit(0), _) | (Mul, _, Lit(0)) => Lit(0)
    | (_, sa, sb) => Bin(op, sa, sb)
    }
  }
