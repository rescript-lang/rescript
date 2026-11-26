module type S = {
  type t
}

let f = (m: module(S with type t = int and type t = string)) => m
