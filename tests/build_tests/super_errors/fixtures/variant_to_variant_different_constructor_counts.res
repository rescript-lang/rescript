type x = One(bool) | Two | Three | Four | Five
type y = One(bool)

let x: x = One(true)

let y = (x :> y)
