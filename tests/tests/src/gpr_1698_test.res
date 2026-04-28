@@config({no_export: true})
type value =
  | Natural(int)
  | Symbol(string)
type context = InSum
type rec t =
  | Val(value)
  | Neg(t)
  | Sum(list<t>)
  | Pow(t, t)
  | Frac(t, t)
  | Gcd(t, t)
let rec is_number = (expr: t) =>
  switch expr {
  | Val(Natural(_)) => true
  | Neg(a) => is_number(a)
  | Val(Symbol(_)) | Sum(_) | Gcd(_) | _ => false
  }

let sym = x => Val(Symbol(x))

let rec compare = (context, state, a, b) =>
  /* Console.log {j|$a vs $b |j}; */
  switch (a, b, context) {
  | (Neg(x), y, _) | (x, Neg(y), _) =>
    /* Console.log ""; */
    compare(context, state, x, y)
  | (Val(_), Val(_), _) => 111
  | (Gcd(na, da), Gcd(nb, db), _)
  | (Frac(na, da), Frac(nb, db), _) =>
    let denom = compare(context, state, da, db)
    denom == 0 ? compare(context, state, na, nb) : denom
  | (Sum(_), y, _) if is_number(y) => 1
  | (x, Sum(_), _) if is_number(x) => -1
  | (Pow(_), _, _) => -1
  | (_, Pow(_), _) => 1
  | (Sum(_), _, _) => -1
  | (_, Sum(_), _) => 1
  | (Gcd(_), _, _) => 1
  | (_, Gcd(_), _) => -1
  | _ => assert(false)
  }
let a = Sum(list{sym("a"), Val(Natural(2))})
let b = sym("x")
type st = {complex: bool}
let empty = () => {complex: true}
let _ = Console.log(compare(InSum, empty(), a, b))
