module type S = {
  type t
  let v: t
}

let extract = (p: module(S with type t = 'a)) => {
  module M = unpack(p)
  M.v
}
