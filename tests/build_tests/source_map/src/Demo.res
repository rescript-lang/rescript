let add = (a, b) => a + b

let crash = () => Js.Exn.raiseError("source map test")

let value = add(20, 22)
