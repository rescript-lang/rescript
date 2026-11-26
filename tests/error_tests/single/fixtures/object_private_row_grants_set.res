/* A signature must not grant write capability its implementation lacks:
   the interface's @set field requires a settable implementation field. The
   reverse direction (implementation @set abstracted to read-only) is legal
   and pinned in tests/tests/src/object_mutability_pin.res. */
module M: {
  type t = private {..@set "x": int}
} = {
  type t = private {.."x": int}
}
