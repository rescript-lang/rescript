/* Checkout [Ctype.arity] [Btype.repr] 
    [Predef]
    
    Note that [f g x] can also be [(f g) x] 
    even though we get a non-function return type
 */

let f0 = (g, x) => g(x)

let f1 = (g, x): unit => g(x)

module X: {
  type t
} = {
  type t = int => int
}

let f2 = (g, x): X.t => g(x)

let f3 = (g, x) => ignore(g(x))

let f4 = (g, x): ('a, 'b) => g(x)

let f5 = (g, x): list<_> => g(x)

let f6 = (g, x): int => g(x)

let f7 = (g, x): (_ => _) => g(x)

module X0 = {
  type t = int => int
}

let f8 = (g, x): X0.t => g(x)

let f9 = (g, x): Abstract_type.t => g(x)

let f10 = (g, x): Abstract_type.arrow_type => g(x)

let f11 = (g, x): Abstract_type.poly_type<_> => g(x)

let f12 = (g, x): Abstract_type.poly_abstract_type<_> => g(x)

module X2: {
  type t
  let f13: ('a => t, 'a) => t
} = {
  type t = int => int
  let f13 = (g, x): t => g(x)
}

let f14 = (h, g, x): unit => h(g, x)
