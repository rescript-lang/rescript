@val @send
external doThing: (string, int) => int = "doThing"

let _ = doThing("x", 1)
