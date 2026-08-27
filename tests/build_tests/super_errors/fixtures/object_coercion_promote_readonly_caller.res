/* Pin (object mutability cleanup): COERCION-driven strengthening (as
   opposed to the assignment-driven case in
   object_open_write_readonly_caller.res): coercing an open-row parameter to
   a same-type mutable target constrains the row, so a read-only caller is
   rejected. Both halves must survive the new model (the coercion promotes
   the open source's field; the demand becomes a Mutable field).
   See docs/object_representation_cleanup.md. */
type wide = {"a": int, "b": int}
let f = (o: {.."x": wide}) => (o :> {@set "x": wide})
@val external readonly: {"x": wide} = "readonly"
let _ = f(readonly)
