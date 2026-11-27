@unboxed type x = One(bool) | Two
type y = One(bool) | Two

let x: x = One(true)

let y = (x :> y)
