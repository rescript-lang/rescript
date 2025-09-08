type t

@send external add: (t, t) => t = "add"
@send external sub: (t, t) => t = "sub"
@send external mul: (t, t) => t = "mul"
@send external div: (t, t) => t = "div"
@send external gt: (t, t) => bool = "gt"
@send external lt: (t, t) => bool = "lt"
@send external eq: (t, t) => bool = "eq"
@send external cmp: (t, t) => int = "cmp"
@send external sqr: t => t = "sqr"
@send external toString: t => string = "toString"
@send external toStringRad: (t, int) => string = "toString"

@send external toNumber: t => int = "toNumber"
@send external toNumberFloat: t => float = "toNumber"

@new @module("bn.js") external new_: string => t = "default"
@new @module("bn.js") external newInt_: int => t = "default"

// [@module "@polkadot/util"] external tSqrt: (. t) => t = "tSqrt";

// let test = tSqrt(. new_("50"));
