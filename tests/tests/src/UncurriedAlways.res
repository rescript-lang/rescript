@@uncurried

let foo = (x, y) => x + y

let z = foo(3, 4)

let bar = (x, y) => x + y

let b = bar(3, 4)

let w = 3->foo(4)

let a = 3->foo(4)

Console.log(a) // Test automatic uncurried application

let _ = Js.Array2.map([1], x => x + 1)

let ptl = foo(10, ...) // force partial application

let foo2 = (x, y) => x + y
let bar2: _ => _ = foo2(_, 3)

let foo3 = (x, y, z) => x + y + z
let bar3: _ => _ = foo3(_, 3, 4)

type cmp = Jsx.component<int>
let q: cmp = _ => Jsx.null // Check that subtyping works past type definitions

@inline
let inl = () => ()

@inline
let inl2 = (x, y) => x + y

module AllLabels = {
  let foo = (~x, ~y, ~z) => (x, y, z)

  let ptl = foo(~y="y", ...)

  let a1 = ptl(~x="x", ~z="z")
  Console.log2("a1:", a1)
}

module OptAtEnd = {
  let foo = (~x, ~y, ~z, ~d="d=0") => (x, y, z, d)

  let ptl = foo(~y="y", ...)

  let b1 = ptl(~x="x", ~z="z")
  Console.log2("b1:", b1)
  let b2 = ptl(~x="x", ~z="z", ~d="d<-100")
  Console.log2("b2:", b2)
}

module OptMixed = {
  let foo = (~d1="d1=0", ~x, ~d2="d2=0", ~y, ~d3="d3=0", ~z, ~d4="d4=0", ~w, ~d5="d5=0") => (
    d1,
    x,
    d2,
    y,
    d3,
    z,
    d4,
    w,
    d5,
  )

  let ptl = foo(~y="y", ~w="w", ...)

  let c1 = ptl(~x="x", ~z="z")
  Console.log2("c1:", c1)
  let c2 = ptl(~x="x", ~z="z", ~d1="d1<-100")
  Console.log2("c2:", c2)
  let c3 = ptl(~x="x", ~z="z", ~d2="d2<-200", ~d4="d4<-400")
  Console.log2("c3:", c3)
}

let fn = cb => {
  cb()
}

fn(s => Console.log(#foo(s)))

let fn1 = (a, b, ()) => a() + b

let a = fn1(() => 1, 2, _)

module PartialApplication = {
  let f3 = (~x, ~y, ~z) => {
    Console.log(x)
    x + y + z
  }

  let fx = f3(~x=1, ...)

  let fy = f3(~y=1, ...)

  let fz = f3(~z=1, ...)

  let fxyz = f3(~x=1, ~y=1, ~z=1, ...)
}

module ReverseApplication = {
  let hello1 = (y, f) => f(y)
  let hello2 = (y, f) => f(y)
}

module Pipe = {
  let f = (a, b, c) => a->(b, c)

  let f2 = (a, b, c, d, e) => a(b)->(c(d), d(1, 2), e)->(((u, v, h)) => u + v + h)

  let f3 = (foo, x) => foo(x)

  let f4 = (x, f) => x->f(3)
}
