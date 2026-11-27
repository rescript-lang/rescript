@unboxed
type x = One | Two | Other(float)

let x = One

let y = (x :> string)
