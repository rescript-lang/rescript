let myFn = (a, b) => a ++ b->Int.toString

type fnType = %typeof(myFn)

let f: fnType = myFn
let ff: fnType = (a, b) => a->Int.toString + b
