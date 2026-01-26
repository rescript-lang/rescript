module type SomeModule = {
  module Inner: {
    let v: int
  }
  type t = {x: int}
  let foo: t => int
  let doStuff: string => unit
  let doOtherStuff: string => unit
}

let someFn = (~ctx: {"someModule": module(SomeModule)}) => {
  let module(SomeModule) = ctx["someModule"]
  //            ^hov
  //SomeModule.
  //           ^com

  let _ff = SomeModule.doStuff
  //    ^hov

  module M = CompletionFromModule.SomeModule
  // M.
  //   ^com

  // M.g
  //    ^com
  ()
}

// Module type alias + unpack
module type S2 = SomeModule
let testAliasUnpack = (~ctx: {"someModule": module(SomeModule)}) => {
  let module(S2) = ctx["someModule"]
  // S2.
  //    ^com
  ()
}
// Functor param completion
module Functor = (X: SomeModule) => {
  // X.
  //   ^com
  let _u = X.doStuff
}
// First-class type hover without binding via module pattern
let typeHover = (~ctx: {"someModule": module(SomeModule)}) => {
  let v: module(SomeModule) = ctx["someModule"]
  //   ^hov
  ()
}
// Nested unpack inside nested module
module Outer = {
  let nested = (~ctx: {"someModule": module(SomeModule)}) => {
    let module(SomeModule) = ctx["someModule"]
    //SomeModule.
    //           ^com
    ()
  }
}
// Shadowing: inner binding should be used for completion
let shadowing = (
  ~ctx1: {"someModule": module(SomeModule)},
  ~ctx2: {"someModule": module(SomeModule)},
) => {
  let module(SomeModule) = ctx1["someModule"]
  {
    let module(SomeModule) = ctx2["someModule"]
    //SomeModule.
    //           ^com
    ()
  }
}