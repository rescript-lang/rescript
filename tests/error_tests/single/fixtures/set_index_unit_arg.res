type t

@set_index
external setI: (t, string, unit) => unit = ""

let f = (t: t) => setI(t, "k", ())
