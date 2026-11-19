type t

@get_index
external getI: (t, unit) => int = ""

let f = (t: t) => getI(t, ())
