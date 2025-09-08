let x = 1

let xx = (x :> float)

type r1 = {x: int}
type r2 = {x: int}

type t1 = array<r1>
type t2 = array<r2>

let foo = (x: t1) => {(x :> t2)}
