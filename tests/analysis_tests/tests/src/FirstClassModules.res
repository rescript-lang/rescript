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
}
