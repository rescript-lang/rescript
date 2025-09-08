module MyList = Belt.List

module Dep: {
  @ocaml.doc("Some doc comment") @deprecated("Use customDouble instead")
  let customDouble: int => int
} = {
  let customDouble = foo => foo * 2
}

module Lib = {
  let foo = (~age, ~name) => name ++ Int.toString(age)
  let next = (~number=0, ~year) => number + year
}

let op = Some(3)

module ForAuto = {
  type t = int
  let abc = (x: t, _y: int) => x
  let abd = (x: t, _y: int) => x
}

let fa: ForAuto.t = 34

module O = {
  module Comp = {
    @react.component
    let make = (~first="", ~zoo=3, ~second) => React.string(first ++ second ++ Int.toString(zoo))
  }
}

let zzz = 11

//^doc
