type x = | @as(1.1) One(bool) | @as(2.2) Two

let x = One(true)

let y = (x :> float)
