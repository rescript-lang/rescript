module MyList = List
// MyList.m
//         ^com
// Array.
//       ^com
// Array.m
//        ^com

module Dep: {
  @ocaml.doc("Some doc comment") @deprecated("Use customDouble instead")
  let customDouble: int => int
} = {
  let customDouble = foo => foo * 2
}

// let cc = Dep.c
//               ^com

module Lib = {
  let foo = (~age, ~name) => name ++ Int.toString(age)
  let next = (~number=0, ~year) => number + year
}

// let x = Lib.foo(~
//                  ^com

// [1,2,3]->m
//           ^com

// "abc"->toU
//           ^com

let op = Some(3)

// op->e
//      ^com

module ForAuto = {
  type t = int
  let abc = (x: t, _y: int) => x
  let abd = (x: t, _y: int) => x
}

let fa: ForAuto.t = 34
// fa->
//     ^com

// "hello"->String.in
//                   ^com

module O = {
  module Comp = {
    @react.component
    let make = (~first="", ~zoo=3, ~second) => React.string(first ++ second ++ Int.toString(zoo))
  }
}

let zzz = 11

// let comp = <O.Comp second=z
//                            ^com

// let comp = <O.Comp z
//                     ^com

// @reac
//      ^com

// @react.
//        ^com

// let x = Lib.foo(~name, ~
//                         ^com

// let x = Lib.foo(~age, ~
//                        ^com

// let x = Lib.foo(~age={3+4}, ~
//                              ^com

let _ = Lib.foo(
  //~age,
  //~
  // ^com
  ~age=3,
  ~name="",
)

let someObj = {"name": "a", "age": 32}

// someObj["a
//           ^com

let nestedObj = {"x": {"y": {"name": "a", "age": 32}}}

// nestedObj["x"]["y"]["
//                      ^com

let o: Objects.objT = assert(false)
// o["a
//     ^com

type nestedObjT = {"x": Objects.nestedObjT}
let no: nestedObjT = assert(false)
// no["x"]["y"]["
//               ^com

type r = {x: int, y: string}
type rAlias = r
let r: rAlias = assert(false)
// r.
//   ^com

// Objects.Rec.recordVal.
//                       ^com

let myAmazingFunction = (x, y) => x + y

@react.component
let make = () => {
  // my
  //   ^com
  <> </>
}

// Objects.object["
//                 ^com

let foo = {
  let x = {
    3
  }
  let y = 4
  let add = (a, b) =>
    switch a {
    | 3 => a + b
    | _ => 42
    }
  let z = assert(false)
  let _ = z
  module Inner = {
    type z = int
    let v = 44
  }
  exception MyException(int, string, float, array<JSON.t>)
  let _ = raise(MyException(2, "", 1.0, []))
  add((x: Inner.z), Inner.v + y)
}

exception MyOtherException

// <O.
//    ^com

type aa = {x: int, name: string}
type bb = {aa: aa, w: int}
let q: bb = assert(false)
// q.aa.
//      ^com
// q.aa.n
//       ^com

// Lis
//    ^com

module WithChildren = {
  @react.component
  let make = (~children, ~name as _: string) => <jsx> children </jsx>
}
// <WithChildren
//              ^com

// type t = Null.
//               ^com
// type t = ForAuto.
//                  ^com

type z = Allo | Asterix | Baba

// let q = As
//           ^com

// module M = For
//               ^com

module Private = {
  %%private(let awr = 3)
  let b = awr
}

// Private.
//         ^com

module Shadow = {
  module A = {
    let shadowed = 3
  }
  module B = {
    let shadowed = ""
  }
}

// sha
//    ^com
open Shadow.A
// sha
//    ^com
open Shadow.B
// sha
//    ^com
let _ = shadowed

module FAR = {
  type forAutoRecord = {forAuto: ForAuto.t, something: option<int>}
  let forAutoRecord: forAutoRecord = assert(false)
}

module FAO = {
  let forAutoObject = {"forAutoLabel": FAR.forAutoRecord, "age": 32}
}

// FAO.forAutoObject["
//                    ^com

// FAO.forAutoObject["forAutoLabel"].
//                                   ^com

// FAO.forAutoObject["forAutoLabel"].forAuto->
//                                            ^com

// FAO.forAutoObject["forAutoLabel"].forAuto->ForAuto.a
//                                                     ^com

let name = "abc"
// let template = `My name is ${na}`
//                                ^com

let notHere = "      "
//               ^com

let someR = Some(r)
let _ = switch someR {
| Some(_z) => 1
// + _z.
//      ^com
| _ => 3
}

module SomeLocalModule = {
  let aa = 10
  let bb = 20
  type zz = int
}

// let _ = SomeLo
//               ^com
// type zz = SomeLocalModule.
//                           ^com

type record = {
  someProp: string,
  //  otherProp: SomeLocalModule.
  //                             ^com
  thirdProp: string,
}

type someLocalVariant = SomeLocalVariantItem

// type t = SomeLocal
//                   ^com

// let _ : SomeLocal
//                  ^com

let _foo = _world => {
  // let _ = _w
  //           ^com
  3
}

type someType = {hello: string}
// type t = SomeType(s)
//                    ^com

type funRecord = {
  someFun: (~name: string) => unit,
  stuff: string,
}

let funRecord: funRecord = assert(false)

// let _ = funRecord.someFun(~ )
//                            ^com

let retAA = () => {x: 3, name: ""}

// retAA().
//         ^com

let ff = (~opt1=0, ~a, ~b, (), ~opt2=0, (), ~c) => a + b + c + opt1 + opt2

// ff(~c=1)(~
//           ^com

// ff(~c=1)()(~
//             ^com

// ff(~c=1, ())(~
//               ^com

// ff(~c=1, (), ())(~
//                   ^com

// ff(~c=1, (), ~b=1)(~
//                     ^com

// ff(~opt2=1)(~
//              ^com

type callback = (~a: int) => int

let withCallback: (~b: int) => callback = (~b) => {
  ()
  (~a) => a + b
}

// withCallback(~
//               ^com

// withCallback(~a)(~
//                   ^com

// withCallback(~b)(~
//                   ^com

let _ =
  <div
    onClick={_ => {
      ()
      //        let _: Res
      //                  ^com
    }}
    name="abc">
    {React.string(name)}
  </div>

//let _ = switch Some(3) { | Some(thisIsNotSaved) -> this
//                                                       ^com

let _ = <div name="" />
//            ^hov

// let _ = FAO.forAutoObject["age"]
//               ^hov

// let _ = ff(~opt1=3)
//               ^hov

// (let _ = ff(~opt1=3))
//                     ^com

type v = This | That

let _ = x =>
  switch x {
  // | T
  //    ^com
  | _ => 4
  }

module AndThatOther = {
  type v = And | ThatOther
}

let _ = x =>
  switch x {
  // | AndThatOther.T
  //                 ^com
  | _ => 4
  }

// let _  = ` ${ForAuto.}`
//                      ^com

// let _  = `abc ${FAO.forAutoObject[""}`
//                                    ^com

// let _ = `${funRecord.}`
//                      ^com

let _ = _ => {
  //  []->ma
  //        ^com
  ()
}

let red = "#ff0000"

let header1 = `
    color: ${red}; `
//            ^com

let header2 = `
    color: ${red};
    background-color: ${red}; `
//                       ^com

// let _ = `color: ${r
//                    ^com

let onClick = evt => {
  // SomeLocalModule.
  //                 ^com
  evt->ReactEvent.Synthetic.preventDefault
  // SomeLocalModule.
  //                 ^com
  Console.log("Hello")
}

// let _ = 123->t
//               ^com

// let _ = 123.0->t
//                 ^com

let ok = Ok(true)

// ok->g
//      ^com

type someRecordWithDeprecatedField = {
  name: string,
  @deprecated
  someInt: int,
  @deprecated("Use 'someInt'.")
  someFloat: float,
}

let rWithDepr: someRecordWithDeprecatedField = {
  name: "hej",
  someInt: 12,
  someFloat: 12.,
}

// Should show deprecated status
// rWithDepr.so
//             ^com

type someVariantWithDeprecated =
  | @deprecated DoNotUseMe | UseMeInstead | @deprecated("Use 'UseMeInstead'") AndNotMe

// Should show deprecated status
// let v: someVariantWithDeprecated =
//                                   ^com

let uncurried = num => num + 2

// let _ = uncurried(. 1)->toS
//                            ^com

type withUncurried = {fn: int => unit}

// let f: withUncurried = {fn: }
//                            ^com

// let someRecord = { FAR. }
//                        ^com
