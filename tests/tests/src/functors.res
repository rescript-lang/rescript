module type S = {
  let foo: int => int
}

@inline(always)
module O = (X: S) => {
  let cow = x => X.foo(x)
  let sheep = x => 1 + cow(x)
}

@inline(always)
module F = (X: S, Y: S) => {
  let cow = x => Y.foo(X.foo(x))
  let sheep = x => 1 + cow(x)
}

module type S1 = {
  let bar: int => int
  let foo: int => int
}

module type T = {
  let sheep: int => int
}

@inline(always)
module F1 = (X: S, Y: S): T => {
  let cow = x => Y.foo(X.foo(x))
  let sheep = x => 1 + cow(x)
}

@inline(always)
module F2: (S1, S1) => T = (X: S, Y: S) => {
  let cow = x => Y.foo(X.foo(x))
  let sheep = x => 1 + cow(x)
}

module M: {
  module F: (X: S1, Y: S1) => T
} = {
  @inline(always)
  module F = (X: S, Y: S) => {
    let cow = x => Y.foo(X.foo(x))
    let sheep = x => 1 + cow(x)
  }
}
