/* Pin (object mutability cleanup): a closed read-only field cannot be
   coerced to a settable one — write capability cannot be conjured. Must
   stay an error under the new model (closed row: no promotion).
   See docs/object_representation_cleanup.md. */
type t = {"x": int}
let p = (v: t) => (v :> {@set "x": int})
