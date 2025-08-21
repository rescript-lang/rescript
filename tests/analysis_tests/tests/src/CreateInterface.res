// ^int

type r = {name: string, age: int}

let add = (~x, ~y) => x + y

@react.component
let make = (~name) => React.string(name)

module Other = {
  @react.component
  let otherComponentName = (~name) => React.string(name)
}

module Mod = {
  @react.component
  let make = (~name) => React.string(name)
}

module type ModTyp = {
  @react.component
  let make: (~name: string) => React.element
}

@module("path") external dirname: string => string = "dirname"

@module("path") @variadic
external join: array<string> => string = "join"

@val
external padLeft: (
  string,
  @unwrap
  [
    | #Str(string)
    | #Int(int)
  ],
) => string = "padLeft"

@inline
let f1 = 10

@inline let f2 = "some string"

@genType @inline
let f3 = 10

@genType @inline
let f4 = "some string"

@genType @inline let f5 = 5.5

module RFS = {
  @module("fs")
  external readFileSync: (
    ~name: string,
    @string
    [
      | #utf8
      | @as("ascii") #useAscii
    ],
  ) => string = "readFileSync"
}

module Functor = () => {
  @react.component
  let make = () => React.null
}

module type FT = {
  module Functor: (
    X: {
      let a: int
      @react.component
      let make: (~name: string) => React.element
      let b: int
    },
    Y: ModTyp,
  ) =>
  {
    @react.component
    let make: (~name: string) => React.element
  }
}

module NormaList = List
open Belt
module BeltList = List

module type MT2 = ModTyp

module rec RM: ModTyp = D
and D: ModTyp = Mod

module type OptT = {
  @react.component
  let withOpt1: (~x: int=?, ~y: int) => int

  module type Opt2 = {
    @react.component
    let withOpt2: (~x: int=?, ~y: int) => int
  }

  module type Opt3 = {
    @react.component
    let withOpt3: (~x: option<int>, ~y: int) => int
  }
}

module Opt = {
  @react.component
  let withOpt1 = (~x=3, ~y) => x + y

  module Opt2 = {
    @react.component
    let withOpt2 = (~x: option<int>=?, ~y: int) =>
      switch x {
      | None => 0
      | Some(x) => x
      } +
      y
  }
  module type Opt2 = module type of Opt2

  module Opt3 = {
    @react.component
    let withOpt3 = (~x: option<int>, ~y: int) =>
      switch x {
      | None => 0
      | Some(x) => x
      } +
      y
  }
  module type Opt3 = module type of Opt3
}

module Opt2: OptT = Opt
module Opt3 = Opt

module Memo = {
  @react.component
  let make = (~name) => React.string(name)

  let make = React.memo(make)
}

external x: (@as("bar") ~foo: int) => unit = "myexternal"

type record = {
  @as("foo_bar")
  fooBar: int
}
