module type S = {
  module M: {
    let f: unit => unit
  }
}

module Make = (X: {}) => {
  module M = {
    let f = () => ()
  }
}

module rec A: {} = {}
and B: S = {
  module C = Make(A)
  include C
}
