@throws(Not_found)
let lookup: int => int

@throws(JsExn)
external parseIface: string => int = "parseThrowing"

// The interface has no @throws, and it is what the .cmi holds.
let implOnly: int => int
