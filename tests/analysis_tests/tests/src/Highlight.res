module M = {
  module C = Component
}

let _c = <Component />

let _mc = <M.C />

let _d = <div />

let _d2 =
  <div>
    {React.string("abc")}
    <div> {React.string("abc")} </div>
    {React.string("abc")}
    {React.string("abc")}
  </div>

type pair<'x, 'y> = ('x, 'y)

type looooooooooooooooooooooooooooooooooooooong_int = int

type looooooooooooooooooooooooooooooooooooooong_string = string

type pairIntString = list<
  pair<
    looooooooooooooooooooooooooooooooooooooong_int,
    looooooooooooooooooooooooooooooooooooooong_string,
  >,
>

let _ = !(3 < 4) || 3 > 4

module type MT = {
  module DDF: {

  }
}

module DDF: MT = {
  module DDF = {

  }
}

module XX = {
  module YY = {
    type t = int
  }
}

open XX.YY

type tt = t

// ^hig

module T = {
  type someRecord<'typeParameter> = {
    someField: int,
    someOtherField: string,
    theParam: 'typeParameter,
  }

  type someEnum = A | B | C
}

let foo = x => x.T.someField

let add = (~hello as x, ~world) => x + world

let _ = add(~hello=3, ...)

let _ = <div scale="abc"> <div /> </div>

module SomeComponent = {
  module Nested = {
    @react.component
    let make = (~children) => {
      <> {children} </>
    }
  }
}

let _ = <SomeComponent.Nested> <div /> </SomeComponent.Nested>

// true/false
let _ = true || false

// to/downto as label
let toAs = (~to as x) => x
let _toEquals = toAs(~to=10)

let to = 1
for _ in to + to to to + to {
  ()
}

module ToAsProp = {
  @react.component
  let make = (~to) => {
    <> {React.int(to)} </>
  }
}
let _ = <ToAsProp to=3 />

// quoted identifiers
let \"true" = 4
let _ = \"true"

let enumInModule = T.A

type typeInModule = XX.YY.t

module QQ = {
  type somePolyEnumType = [
    | #someMember
    | #AnotherMember
    | #SomeMemberWithPayload(list<int>)
    | #"fourth Member"
  ]
}

let _ = x =>
  switch x {
  | #stuff => 3
  | #...QQ.somePolyEnumType => 4
  }

let _ = 3 == 3 || 3 === 3

let _ = (~_type_ as _) => ()

let _ = {"abc": 34}

let _ = {"Key": 2}
