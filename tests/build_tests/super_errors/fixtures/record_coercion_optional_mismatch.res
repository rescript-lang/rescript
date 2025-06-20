type x = {
  x?: int,
  y: int,
}
type y = {
  x: int,
  y: int,
}

let x: x = {
  y: 1,
}

let y = (x :> y)
