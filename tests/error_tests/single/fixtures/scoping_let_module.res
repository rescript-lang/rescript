let leaked = {
  module M: {
    type t
    let v: t
  } = {
    type t = int
    let v: t = 1
  }
  M.v
}

let _ = leaked
