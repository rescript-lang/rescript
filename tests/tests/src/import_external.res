@module("a")
external makeA: string = "default"

let f8 = import(makeA)

@module("b")
external makeB: string => unit = "default"

let f9 = import(makeB)

@val @scope("Math") @module("m") external scoped: float => float = "floor"

let f10 = import(scoped)

/* non-identity bindings get their FFI adaptation inside the import: the
 imported value honestly has the external's ReScript type */
@module("m") @variadic external sum: array<int> => int = "sum"
let f11 = import(sum)

@module("m") @return(nullable) external getUser: string => option<string> = "getUser"
let f12 = import(getUser)
