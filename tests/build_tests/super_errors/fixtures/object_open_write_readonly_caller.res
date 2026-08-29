/* Writing a field in an open row promotes it to mutable. The promotion is
   visible in the function's parameter type, which requires callers to supply
   a mutable field. A closed read-only object cannot satisfy that strengthened
   parameter. */

let f = (o: {.."x": int}) => o["x"] = 1
@val external readonly: {"x": int} = "readonly"
let _ = f(readonly)
