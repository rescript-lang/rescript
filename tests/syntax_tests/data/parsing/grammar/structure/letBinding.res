let a = 1

@onFirstBinding
let a = 1

@onFirstBinding
let a = 1
@onSecondBinding
and b = 2

// locally abstract types
let f: type t. foo<t> = (sideEffect) => {
  module M = { exception E(t) }
  sideEffect()
  x => M.E(x)
}

let f: type t x u. list<(t, x, y)> = (l) => f(l)
