let a = 12

let b = [1, 2, 3, a]

let c = <div />

let s = React.string

module M = {
  @react.component
  let make = (~x) => React.string(x)
}

let d = <M x="abc" />

module J = {
  @react.component
  export make = (~children: React.element) => React.null
}

let z = <J> {React.string("")} {React.string("")} </J>

type inline =
  | A({x: int, y: string})
  | B({x: int, y: string})
  | C({
      x: int,
      y: string,
      z: string,
      w: string,
      x0: string,
      q1: string,
      q2: string,
      q3: string,
      q4: string,
    })
  | D({x: int, y: string})
  | E({x: int, y: string})
  | F

module MSig: {
  type rec t = A(list<s>)
  and s = list<t>

  let x: int
} = {
  type rec t = A(list<s>)
  and s = list<t>

  let x = 14
}

module Impl = {
  type rec t = A(list<s>)
  and s = list<t>

  type w = int

  let x = 14
}

module Impl2 = {
  include Impl
}

module D = MSig
module E = Impl
module F = Impl2

@ocaml.doc("str docstring")
type str = string

@ocaml.doc("gr docstring")
type gr = {x: int, s: str}

let testRecordFields = (gr: gr) => {
  let str = gr.s
  str
}

@ocaml.doc("vr docstring")
type vr = V1 | V2

let v1 = V1

module DoubleNested = ModuleWithDocComment.Nested.NestedAgain

let uncurried = (. x) => x + 1

module Inner = {
  type tInner = int
  let vInner = 34
}

type typeInner = Inner.tInner

let valueInner = Inner.vInner

@ocaml.doc("Doc comment for functionWithTypeAnnotation")
let functionWithTypeAnnotation: unit => int = () => 1

module HoverInsideModuleWithComponent = {
  let x = 2 // check that hover on x works

  @react.component
  let make = () => React.null
}

module Lib = {
  let foo = (~age, ~name) => name ++ string_of_int(age)
  let next = (~number=0, ~year) => number + year
}

@ocaml.doc("This module is commented") @deprecated("This module is deprecated")
module Dep: {
  @ocaml.doc("Some doc comment") @deprecated("Use customDouble instead")
  let customDouble: int => int

  let customDouble2: int => int
} = {
  let customDouble = foo => foo * 2
  let customDouble2 = foo => foo * 2
}

let cc = Dep.customDouble(11)

module O = {
  module Comp = {
    @react.component
    let make = (~first="", ~kas=11, ~foo=3, ~second, ~v) =>
      React.string(first ++ second ++ string_of_int(foo))
  }
}

let comp = <O.Comp key="12" second="abcc" v=12 />

let lll = List.make(3, 4)

let abc = "abc"

let arr = [1, 2, 3]

let some7 = Some(7)


