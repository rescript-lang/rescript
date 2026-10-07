let f = x => x + 1

let a = (@inlined f)(1)
let b = @inlined f(2)

module F = (X: {}) => {}
module M = @inlined F({})
