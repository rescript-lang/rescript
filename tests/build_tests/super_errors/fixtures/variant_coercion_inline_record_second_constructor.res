type v1 = A(int) | B({x: float}) | C({y: int})
type v2 = A(int) | B({x: float}) | C({mutable y: int}) | D

let toV2 = (a: v1) => (a :> v2)
