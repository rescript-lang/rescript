module M = {
  type v1 = A({@as("z") x: float}) | B({y: int})
  type v2 = A({x: float}) | B({y: int})
}

let toV2 = (a: M.v1) => (a :> M.v2)
