@@jsxConfig({version: 4})

module V4A = {
  @react.component
  let make = (type a, ~a: a, ~b: array<option<[#Foo(a)]>>, ~c: 'a, _) => <div />
}

module V4A1 = {
  @react.component
  let make = (type x y, ~a:x, ~b:array<y>, ~c:'a) => <div />
}

module type T = {
  type t
}

module V4A2 = {
  @react.component
  let make = (type a, ~foo: module(T with type t = a)) => {
    module T = unpack(foo)
    <div />
  }
}

module V4A3 = {
  @react.component
  let make = (type a, ~foo) => {
    module T = unpack(foo: T with type t = a)
    foo
  }
}

@react.component
let make =(~x : ('a,'b), ~q:'a ) => [fst(x), q]

@@uncurried

module Uncurried = {
  @react.component
  let make = (type a, ~foo=?) => React.null}
