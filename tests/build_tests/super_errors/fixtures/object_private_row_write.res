/* A private row is not an inferred open row: writing a field that is
   not @set must be rejected. The Tconstr terminator shares the
   declaration's mutability cell, so a successful write would persist
   @set into the .cmi. The compiling counterpart (private {..@set}) is
   pinned in tests/tests/src/object_mutability_pin.res. */
type t = private {.."x": int}
let write = (o: t) => o["x"] = 1
