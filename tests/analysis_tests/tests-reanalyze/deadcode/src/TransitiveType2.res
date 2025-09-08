@genType
type t2 = option<TransitiveType3.t3>

@genType
type t2Alias = t2

let convertT2 = (x: t2) => x

