module Make = (T: {}, Q: {}) => {
  module Eq = (E: {}, A: {}) => {}
}

module M = Make((), ())

module EQ = M.Eq((), ())

module MF: {
  module F: (X: {}, Y: {}) => {}
} = {
  module F = (X: {}, Y: {}) => {
    let c = 12
  }
}

module UseF = (X: {}, Y: {}) => MF.F(X, Y)
