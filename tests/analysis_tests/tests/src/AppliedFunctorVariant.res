module type T = {
  type t
}

module Make = (X: T) => {
  type shape = Dot | Box(X.t)
}
