external f: (%raw(`"hello"`), int) => unit = "f"

let x = "\h\e\l\lo"
let y = f(42)
