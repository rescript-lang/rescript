@module("a")
external makeA: string = "default"

let f8 = import(makeA)

@module("b")
external makeB: string => unit = "default"

let f9 = import(makeB)
