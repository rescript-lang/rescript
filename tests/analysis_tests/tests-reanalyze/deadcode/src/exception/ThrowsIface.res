let lookup = x => x
external parseIface: string => int = "parseThrowing"

@throws(Not_found)
let implOnly = x => x > 0 ? x : throw(Not_found)
