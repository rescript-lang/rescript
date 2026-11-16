// A shorthand names a variable, and a reserved word can't be one
let a = {type}
let a = {x, type}
let a = {x, ?type}
let {x, ?type} = r
type t = {type}
let f = (~type) => 1
let f = (~type: int) => 1
let f = (~type=1) => 1
let a = f(~type)
let a = f(~type?)
let a = f(~type: int)
let a = <input type />
let a = <input ?type />

// Still errors: a reserved word as a variable, a value or a qualified tag
let a = <M.type />
let type = 1
let a = M.type
