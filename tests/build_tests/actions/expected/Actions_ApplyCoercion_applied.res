type x1 = One
type x2 = | ...x1 | Two

let x1: x1 = One
let x2: x2 = (x1 :> x2)

/* === AVAILABLE ACTIONS:
- ApplyCoercion(x2) - Coerce to x2
*/
