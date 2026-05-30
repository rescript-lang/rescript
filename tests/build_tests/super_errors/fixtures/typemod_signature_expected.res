module type T = {
  module M: (X: {}) => {}
}
module type T2 = T with type M.t = int
