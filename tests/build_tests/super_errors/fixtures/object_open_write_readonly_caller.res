/* Pin (object mutability cleanup): writing a bare field of an OPEN row is
   accepted but strengthens the function's demand — callers must supply a
   writable field, so a read-only argument is rejected. Both halves must
   survive the new model (write = promotion on the open row; the demand
   becomes a Mutable field). See docs/object_representation_cleanup.md. */
let f = (o: {.."x": int}) => o["x"] = 1
@val external readonly: {"x": int} = "readonly"
let _ = f(readonly)
