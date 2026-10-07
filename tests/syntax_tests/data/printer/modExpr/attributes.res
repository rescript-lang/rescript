module A = @attr F
module B = @attr {}
module C = @attr {
  let x = 1
}
module D = @a @b F

// on a functor application, its functor, or an argument
module E = @attr F({})
module G = (@attr F)({})
module H = @attr F(A, B)
module I = (@attr F(A))(B)
module J = F(@attr X)

module K: T = @attr X
include @attr F({})
module L = @attr unpack(x)
module M = @attr %ext

let f = async () => {
  module A = await @attr X
  module B = await (@attr X: S)
  ()
}
