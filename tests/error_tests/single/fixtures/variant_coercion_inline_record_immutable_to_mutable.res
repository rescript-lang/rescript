type v1 = A({x: float})
type v2 = A({mutable x: float})

let toV2 = (a: v1) => (a :> v2)
