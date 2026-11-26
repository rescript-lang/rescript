type v1 = B | A({x: float})
type v2 = B | A({x?: float})

let toV2 = (a: v1) => (a :> v2)
