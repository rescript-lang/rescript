// actionFilter=PartiallyApplyFunction
let x = (~a, ~b) => a + b
let y = x(~a=2, ...) + 2

/* === AVAILABLE ACTIONS:
- PartiallyApplyFunction - Partially apply function
*/
