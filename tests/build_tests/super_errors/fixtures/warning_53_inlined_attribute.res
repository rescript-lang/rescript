let f = x => x + 1

let a = (@inlined f)(1)
let b = @inlined f(2)

external id: int => int = "%identity"
let c = (@inlined id)(3)
