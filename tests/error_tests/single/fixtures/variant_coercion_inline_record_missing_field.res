type v1 = A({x: float})
type v2 = A({x: float, y: int})

let toV2 = (a: v1) => (a :> v2)
