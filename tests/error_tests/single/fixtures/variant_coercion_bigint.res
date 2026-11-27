type x = | @as(1n) One(bool) | @as(2n) Two

let x = One(true)

let y = (x :> bigint)
