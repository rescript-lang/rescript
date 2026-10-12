@unboxed type special = | @as("special") Special
// Variables could carry the special string; reject at the type declaration.
type broad = {kind: string, value: int}
type narrow = {kind: special, value: float}
@unboxed type t = Broad(broad) | Narrow(narrow)
let make = (value: broad) => Broad(value)
