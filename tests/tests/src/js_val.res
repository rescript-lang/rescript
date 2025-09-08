@val external u: int = "u"

@val @module("x") external vv: int = "vv"

@val @module(("x", "U")) external vvv: int = "vv"
@module(("x", "U")) external vvvv: int = "vvvv"

/* TODO: unify all [module] name, here ideally,
 we should have only one [require("x")] here */
let h = u
let hh = vv
let hhh = vvv
let hhhh = vvvv
