type point = {x: float, y: float}

let f = point => {
  let {x, y} = point
  (x *. x +. y *. y) ** 2.
}
