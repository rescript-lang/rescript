//
module SomeModule = {
  type t = {name: string}

  @get external getName: t => string = "name"
  @send
  external withUnlabelledArgumentNotFirst: (~name: string=?, t) => unit =
    "withUnlabelledArgumentNotFirst"

  let thisShouldNotBeCompletedFor = () => "hi"
}

let n = {SomeModule.name: "hello"}

// Type from inside of a module
// n.
//   ^com

@editor.completeFrom(DotPipeCompletionSpec.SomeOtherModule)
type typeOutsideModule = {nname: string}

let doWithTypeOutsideModule = (_: typeOutsideModule) => ""

module CompleteFromThisToo = {
  external a: typeOutsideModule => string = "a"
  external b: unit => typeOutsideModule = "b"
}

module SomeOtherModule = {
  @editor.completeFrom(DotPipeCompletionSpec.CompleteFromThisToo)
  type t = typeOutsideModule

  type irrelevantType = string

  @get external getNName: t => string = "nname"
  @get external getNName2: typeOutsideModule => string = "nname"
  @get external getNName3: irrelevantType => string = "nname"

  let thisShouldNotBeCompletedFor = () => "hi"
}

let nn: SomeOtherModule.t = {nname: "hello"}

// Type from module but that's an alias
// nn.
//    ^com

module A = {
  @editor.completeFrom(B)
  type a

  external withA: a => unit = "withA"
  external make: unit => a = "makeA"
}

module B = {
  let b = (_a: A.a) => 1
}

external a: A.a = "a"

// Main type in other module
// a.
//   ^com

let xx: CompletionFromModule.SomeModule.t = {name: "hello"}
// Type from other file
// xx.
//    ^com

type builtinType = array<string>

let ffff: builtinType = []

// A built in type
// ffff.u
//       ^com

// Type outside of module with complete from pointing to other module
let nnn: typeOutsideModule = {nname: "hello"}
// nnn.
//     ^com

// Continuous completion
let xxxx = [1, 2]

// xxxx->Js.Array2.filter(v => v > 10).filt
//                                         ^com

// xxxx->Js.Array2.filter(v => v > 10)->Js.Array2.joinWith(",").includ
//                                                                    ^com

let str = "hello"

// str->Js.String2.toLowerCase.toUpperCa
//                                      ^com

// str->Js.String2.toLowerCase->Js.String2.toUpperCase.toLowerC
//                                                             ^com

let cc = (t: typeOutsideModule) => {
  // t.
  //   ^com
  t
}

let outOfScope = (t: typeOutsideModule) => t

// @editor.completeFrom(Dot) type t
//                         ^com

// @editor.completeFrom([CompletionPipe]) type t
//                                     ^com

// @editor.completeFrom([CompletionPipe, Dot]) type t
//                                          ^com

let someObj = {
  "name": "hello",
  "age": 123,
}

// someObj.
//         ^com

// someObj.na
//           ^com

module DOMAPI = {
  type htmlElement = {prefix: string}

  @editor.completeFrom(DotPipeCompletionSpec.HTMLButtonElement)
  type rec htmlButtonElement = {mutable disabled: bool}
}

module HTMLButtonElement = {
  open DOMAPI

  @send
  external checkValidity: htmlButtonElement => bool = "checkValidity"
}

external button: DOMAPI.htmlButtonElement = "button"

// button.
//        ^com
