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
module Q = F(@attr {})
include @attr F({type t = int})
include (@attr F)({type t = int})
include F(@attr {type t = int})

module K: T = @attr X
include @attr F({})

// on a module constraint, or on the module it constrains
module N = @attr (X: S)
module O = F(@attr (X: S))
module P = F((@attr X: S))
include @attr (X: S)
include (@attr X: S)

// on a functor's result
module R = (X) => @attr (Y: S)
module S = (X): S => @attr Y

// @JSX is printed like any other attribute
module T = (@JSX F)(A)
module U = (@JSX F(A))(B)
module V = @JSX (X: S)

// a functor argument can't start with a doc comment
module W = H(@w /** doc */ X)
module Y = H((/** doc */ X: S))
module AA = H((/** doc */ (await X)))
module Z = /** doc */ X
include /** doc */ X

module L = @attr unpack(x)
module M = @attr %ext

let f = async () => {
  module A = await @attr X
  module B = await (@attr X: S)
  module C = await @attr (X: S)
  module D = @attr (X: S)
  module E = (await F)(A)
  module G = (await F(A))(B)
  module H = (X: T) => await (Y: S)
  module I = (X: T) => await (Y: S) => {}
  module J = @w (await X)
  module K = await @w (await X)
  module L = H(await {
    let x = 1
  })
  ()
}
