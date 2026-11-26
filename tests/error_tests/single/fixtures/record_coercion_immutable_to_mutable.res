type ro = {x: float}
type rw = {mutable x: float}

let toRw = (r: ro) => (r :> rw)
