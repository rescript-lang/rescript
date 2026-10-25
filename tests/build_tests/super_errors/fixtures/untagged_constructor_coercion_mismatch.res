type a = | @unboxed A(int) | B({x: int})
type b = A(int) | B({x: int}) | C
let x: b = ((A(3): a) :> b)
