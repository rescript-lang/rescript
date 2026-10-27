module type S = {
  type t
}

module M: {
  module F: (X: S) => (S with type t = X.t)
} = {
  module F = (X: S) => {
    type t = int
  }
}
