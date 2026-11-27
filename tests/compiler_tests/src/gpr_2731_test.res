let f = x => x + %raw(`1`)

let a = f(1)
let b = f(2)

let g = () => %raw(`1`)

let c = g()
let d = g()
