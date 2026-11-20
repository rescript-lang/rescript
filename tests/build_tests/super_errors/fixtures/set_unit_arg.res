type t

@set
external setX: (t, unit) => unit = "x"

let f = (t: t) => setX(t, ())
