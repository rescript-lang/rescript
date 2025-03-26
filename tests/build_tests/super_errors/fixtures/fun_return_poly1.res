let f = (_, ~def=3) => assert(false)

let ok = f(1)(2)
let err = f(1, 2)
