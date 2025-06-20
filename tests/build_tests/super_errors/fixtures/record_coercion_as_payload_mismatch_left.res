type x = {
  @as("z") x: int,
  y: int,
}
type y = {
  x: int,
  y: int,
}

let x: x = {
  x: 1,
  y: 1,
}

let y = (x :> y)
