@module("a")
external makeA: string = "default"

let f8 = Js.import(makeA)

@module("b")
external makeB: string => unit = "default"

let f9 = Js.import(makeB)
