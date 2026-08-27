/* Pin (object mutability cleanup): coercing a CLOSED source to an open
   target yields a result whose row tail is instantiated from the source,
   i.e. closed — so a subsequent write is rejected (the error even prints
   the result type as the closed {"x": narrow}). This is what makes the
   covariant closed-source/open-target coercion sound: the result is not
   promotable. Must stay an error under the new model.
   See docs/object_representation_cleanup.md. */
type wide = {"a": int, "b": int}
type narrow = {"a": int}
let p = (v: {"x": wide}) => {
  let r = (v :> {.."x": narrow})
  r["x"] = {"a": 1}
}
