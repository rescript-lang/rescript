let z = x :> int

let z2 = (x :> int)

let foo = (x:int) => (x :> int)

let foo2 = (x:int) => ((x :> int))

let bar = x => (x : t :> int)

let bar2 = x => ((x : t) :> int)

call(~x=y :> int, ~z=w : int :> int, ~a, ~b)

let foo = (~a=(3:int), b) => 34

let foo = (~a=(3:>int), b) => 34

let foo = (~a=(3:int:>int), b) => 34

// THESE SHOULD NOT PARSE: no magic in the syntax
// let x: int :> string = y
// let x :> string = y
// let x : int1 :> int2 = 3 :> int3

let x = (/* c0 */ x /* c1 */ :> /* c2 */ int /* c3 */)
