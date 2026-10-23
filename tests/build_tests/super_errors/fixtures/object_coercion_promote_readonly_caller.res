/* Coercing an open-row parameter to a mutable target of the same field type
   promotes the source field. That promotion is visible in the function's
   parameter type, which now requires a mutable field from callers. A closed
   read-only object cannot satisfy the strengthened parameter. This fixture
   checks promotion through coercion rather than direct assignment, and
   verifies that the strengthened requirement reaches the caller. */

type wide = {"a": int, "b": int}
let f = (o: {.."x": wide}) => (o :> {@set "x": wide})
@val external readonly: {"x": wide} = "readonly"
let _ = f(readonly)
