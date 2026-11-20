module type T = {
  type t
}

module Make = (X: T) => {
  type shape = Dot | Box(X.t)
}

module MakeMore = (X: T) => {
  type lr = {lf: int}
  let made = 1
}
