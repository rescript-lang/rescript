type narrow = {mutable tag: [#A]}
type wide = {mutable tag: [#A | #B]}

let widen = (n: narrow) => (n :> wide)
