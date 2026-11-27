type t1 = {x: int, y: int}
type t2 = {x: int, y: int, z?: int}

let f1 = (v: t1) => v.x
let f2 = (v: t2) => v.x

let v = {x: 3, y: 4}
let res = f2(v) // Check that t2 shadows t1
