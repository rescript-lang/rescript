type t = NotAnException

@throws(Missing)
let f = () => 1

@throws([Not_found, NotAnException])
external g: int => int = "g"

@throws
let h = () => 2

// Warned once, though the pattern binds two names
@throws(AlsoMissing)
let (p, q) = (1, 2)

@warning("-113") @throws(Silenced)
let quiet = () => 3
