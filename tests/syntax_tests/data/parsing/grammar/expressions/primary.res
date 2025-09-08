// field expressions
let x = a.b
let x = a.b.c
let x = H20.Water.water.h
let x = p.Parser.token
let x = p.Lang.Parser.token.pos

// set field
lexbuf.lnum = lexbuf.lnum + 1
parser.lexbuf.lnum = parser.lexbuf.lnum + 1

// array access sugar
let x = arr[0]
let x = arr[x: int]
// multiple
let x = arr[0][1]
let x = arr[x: int][y: int]

// array mutation sugar
arr[0] = a + b

// call expressions
f()
f()()
f(a)
f(a)(a)
f(a,) // trailing comma
f(x : int)
f(a, b, c)
f(a, b, c,) // trailing comma

// labelled arguments
f(~a, ~b=bArg, ~c?, ~d=?expr,)
f(~a, ~b=bArg, ~c?, ~d=?expr,)(~a, ~b=bArg, ~c?, ~d=?expr,)(~a, ~b=bArg, ~c?, ~d=?expr,)

// labelled argumetns with constraints
f(~a=x :int, ~b=? y: int)

// Bucklescript ## sugar
connection["platformId"]
connection["left"]["account"]["accountName"]

// Bucklescript #= sugar
john["age"] = 99
john["son"]["age"] = steve["age"] - 5

// interpret strings correct
dict["\n"] = abc
dict["\""] = dict2["\""]
