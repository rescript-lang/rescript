module type S = {
  type t
  let v: t
}

module F = (
  X: {
    let unused: int
  },
) => {
  let pack: module(S) = module(
    {
      type t = int
      let v = 1
    }
  )
  module M = unpack(pack)
  let _ = M.v
}
