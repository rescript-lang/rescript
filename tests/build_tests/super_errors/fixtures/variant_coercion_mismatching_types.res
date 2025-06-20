type x = One(bool) | Two | Three | Four | Five
type y = {
  x: x,
  y: int,
}

let x: x = One(true)

let y = (x :> y)
