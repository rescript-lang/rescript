module Color: {
  type t = private Red | Green
} = {
  type t = Red | Green
}
let fromPoly = ((#Red: [#Red]) :> Color.t)
