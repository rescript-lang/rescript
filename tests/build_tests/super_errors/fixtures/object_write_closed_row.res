/* Pin (object mutability cleanup): writing a bare field of a CLOSED object
   row is an error — the row cannot acquire a setter. Must stay an error
   under the new model (Immutable field in a closed row cannot be promoted).
   See docs/object_representation_cleanup.md; compiling counterparts in
   tests/tests/src/object_mutability_pin.res. */
let g = (o: {"x": int}) => o["x"] = 1
