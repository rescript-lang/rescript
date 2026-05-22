module type S = {
  type t
}

module type T = S with type missing = int
