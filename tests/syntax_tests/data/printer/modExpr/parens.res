// module constraints where `M: S` would not parse or would mean something else
module A: T = (X: S)
module rec B: T = (X: S)
module type C = module type of (X: S)
let d = module((X: S))
let e = (module((X: S1)): module(S2))
let f = () => {
  module M: T = (X: S)
  ()
}

// functors that are applied
module G = ((X: S))(Z)
module H = ((Y: S) => {})(Z)
module I = ((Y): S => W)(Z)
module J = (%ext)(Z)
module K = %ext(A)(Z)
