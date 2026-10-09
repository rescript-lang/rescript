// `@JSX` and `@res.ternary` written in source are ordinary attributes: the
// parser represents JSX and ternaries with their own nodes. They print, with
// the same parens as any other attribute, here `@foo`.

let a = (@JSX x) + 1
let a = 1 + (@JSX x)
let a = (@JSX x)->g
let a = x->(@JSX g)
let a = (@JSX x)->g(1)
let a = (@JSX f(x)) + 1
let a = (@JSX (a + b)) * c
let a = (@JSX x) == y
let a = (@JSX x) && y
let a = !(@JSX x)
let a = (@JSX x).field
let a = (@JSX x) ? a : b

let b = @res.ternary x
let b = (@res.ternary x) + 1
let b = 1 + (@res.ternary x)
let b = (@res.ternary x)->g
let b = x->(@res.ternary g)
let b = (@res.ternary f(x)) + 1
let b = (@res.ternary (a + b)) * c
let b = (@res.ternary (c ? a : b)) + 1
let b = !(@res.ternary x)
let b = -(@res.ternary x)
let b = (@res.ternary x).field
let b = (@res.ternary x)[0]
let b = (@res.ternary x) ? a : b
let b = c ? (@res.ternary x) : b
let b = f(@res.ternary x)
let b = [@res.ternary x, y]
let b = {a: @res.ternary x}

let c = (@foo x) + 1
let c = (@foo x)->g
let c = x->(@foo g)
let c = (@foo (a + b)) * c
let c = !(@foo x)
