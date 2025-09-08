let cApp = foo(3)
let uApp = foo(. 3)

let cFun = x => 3
let uFun = (.x) => 3
let mixFun = (a, .b, c) => (d, e, f) => (g, .h) => 4
let bracesFun = (. x) => y => x+y
let cFun2 = (x, y) => 3
let uFun2 = (. x, y) => 3

type cTyp = string => int
type uTyp = (. string) => int
type mixTyp = (string, .string, string) => (string, string, string) => (string, .string) => int
type bTyp = (. string) => string => int
type cTyp2 = (string, string) => int
type uTyp2 = (.string, string) => int
type cu = unit => int
type cp = () => int
type cuu = unit => unit => int
type cpu = () => unit => int
type cup = unit => () => int
type cpp = () => () => int
type cu2 = (unit, unit) => unit
type cp2 = ((), ()) => unit
type uu = (. unit) => int
type up = (. ()) => int
type uuu = (. unit) => (. unit) => int
type upu = (. ()) => (. unit) => int
type uup = (. unit) => (. ()) => int
type upp = (. ()) => (. ()) => int
type uu2 = (. unit, unit) => unit
type up2 = (. (), ()) => unit

type cnested = (string => unit) => unit
type unested = (. (. string) => unit) => unit

let uannpoly: (. 'a) => string = xx
let uannint: (. int) => string = xx

let _ = @att (. x)  => 34
let _ = @att async (. x)  => 34
let _ = preserveAttr(@att (. x)  => 34)
let _ = preserveAttr(@att async (. x)  => 34)

let t0 = (type a b, l: list<a>, x: a) => list{x, ...l}
let t1 = (. type a b, l: list<a>, x: a) => list{x, ...l}
let t2 = (type a b, . l: list<a>, x: a) => list{x, ...l}
let t3 = (. type a b, . l: list<a>, x: a) => list{x, ...l}
let t4 = (. type a b) => (l: list<a>, x: a) => list{x, ...l}
let t5 = (type a b) => (. l: list<a>, x: a) => list{x, ...l}
let t6 = (. type a b) => (. l: list<a>, x: a) => list{x, ...l}

type arrowPath1 = (. int) => string
type arrowPath2 = (. I.t) => string
type arrowPath3 = int => string
type arrowPath4 = I.t => string
type callback1 = (. ReactEvent.Mouse.t) => unit as 'callback
type callback2 = (. ReactEvent.Mouse.t) => (unit as 'u)
type callback3 = ((. ReactEvent.Mouse.t) => unit) as 'callback

let cApp = foo(. 3)
let uApp = foo(3)

let cFun = (. x) => 3
let uFun = x => 3
let mixFun = (.a) => (b, c) => (.d, e, f) => (.g) => h => 4
let bracesFun = x => (. y) => x+y
let cFun2 = (. x, y) => 3
let uFun2 = (x, y) => 3
let cFun2Dots = (.x, .y) => 3 // redundant dot on y

type cTyp = (. string) => int
type uTyp = string => int
type mixTyp = (.string) => (string, string) => (.string, string, string, string) => string => int
type bTyp = string => (. string) => int
type cTyp2 = (.string, string) => int
type uTyp2 = (string, string) => int
type cu = (. unit) => int
type cp = (. ()) => int
type cuu = (. unit) => (. unit) => int
type cpu = (. ()) => (. unit) => int
type cup = (. unit) => (. ()) => int
type cpp = (. ()) => (. ()) => int
type cu2 = (. unit, unit) => unit
type cp2 = (. (), ()) => unit
type uu = unit => int
type up = () => int
type uuu = unit => unit => int
type upu = () => unit => int
type uup = unit => () => int
type upp = () => () => int
type uu2 = (unit, unit) => unit
type up2 = ((), ()) => unit

type cnested = (. (. string) => unit) => unit
type unested = (string => unit) => unit

let pipe1 = 3->f

let uannpoly: 'a => string = xx
let uannint: int => string = xx

let _ = @att x  => 34
let _ = @att async x  => 34
let _ = preserveAttr(@att x => 34)
let _ = preserveAttr(@att async x => 34)

let t0 = (type a b, l: list<a>, x: a) => list{x, ...l}
let t1 = (. type a b, l: list<a>, x: a) => list{x, ...l}
let t2 = (type a b, . l: list<a>, x: a) => list{x, ...l}
let t3 = (. type a b, . l: list<a>, x: a) => list{x, ...l}

type arrowPath1 = int => string
type arrowPath2 = I.t => string
type arrowPath3 = (. int) => string
type arrowPath4 = (. I.t) => string
type callback1 = ReactEvent.Mouse.t => unit as 'callback
type callback2 = ReactEvent.Mouse.t => (unit as 'u)
type callback3 = (ReactEvent.Mouse.t => unit) as 'callback
