type t = NotAnException

@throws(Missing)
let f = () => 1

@throws([Not_found, NotAnException])
external g: int => int = "g"

@throws
let h = () => 2
