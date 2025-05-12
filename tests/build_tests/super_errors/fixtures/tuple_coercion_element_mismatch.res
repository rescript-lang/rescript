type tupleOfInts = (int, int)
type tupleOfStrings = (string, string)

let x = (1, 2)
let y = (1, 2, 3)

let z = (x :> tupleOfStrings)
