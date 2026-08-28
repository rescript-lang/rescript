/* Pins the typing behavior of object-field mutability
   (docs/object_representation_cleanup.md).

   Every case in this file must keep compiling. The two cases that the
   cleanup intentionally flipped to errors (a setter acquired at a type
   different from the getter, via coercion or assignment) are pinned as
   super_errors fixtures instead: object_coercion_setter_narrower.res and
   object_setter_type_mismatch.res.

   The rejecting counterparts are pinned in
   tests/build_tests/super_errors/fixtures/object_*.res. */

type wide = {"a": int, "b": int}
type narrow = {"a": int} /* wide <: narrow (width subtyping) */

/* Closed rows: coercion may forget write capability, covariantly.
 (Mutable A :> Immutable B with A <: B.) */
let forget_write_covariant = (v: {@set "x": wide}): {"x": narrow} => (v :> {"x": narrow})

/* Open source, closed immutable target: ordinary covariance. Sound forever:
   the coerced alias is read-only, and a later promotion of the source
   writes at the source's own field type. */
let open_source_covariant = (o: {.."x": wide}): {"x": narrow} => (o :> {"x": narrow})

/* Closed source, open target: covariant; the target's tail is instantiated
 from the (closed) source, so the result is not promotable. */
let closed_source_open_target = (v: {"x": wide}) => (v :> {.."x": narrow})

/* Open source, mutable target at the SAME type: accepted, and constrains
   callers to writable objects (today: absorbs the "x#=" member; new model:
   promotion Immutable -> Mutable at the same type). */
let open_source_promote_same_type = (o: {.."x": wide}): {@set "x": wide} => (o :> {@set "x": wide})

/* Writing a bare field of an open row is accepted and strengthens the
   demand on callers (today: adds "x#=" through the tail; new model:
   promotion). The rejection of a read-only caller is pinned in
   object_open_write_readonly_caller.res. */
let open_row_write = (o: {.."x": int}) => o["x"] = 1

/* A generalized getter accepts both read-only and settable objects. */
let read_x = obj => obj["x"]

@val external settable_obj: {@set "x": wide} = "settableObj"
@val external readonly_obj: {"x": wide} = "readonlyObj"

let read_from_settable = (): wide => read_x(settable_obj)
let read_from_readonly = (): wide => read_x(readonly_obj)

/* A generalized setter's instances are independent: each call site can
 promote at its own field type. */
let set_x = (o, v) => o["x"] = v

@val external int_target: {@set "x": int} = "intTarget"
@val external string_target: {@set "x": string} = "stringTarget"

let set_at_int = () => set_x(int_target, 1)
let set_at_string = () => set_x(string_target, "s")

/* Closed immutable-to-immutable coercion is covariant (matrix pin). */
let closed_immutable_covariant = (v: {"x": wide}): {"x": narrow} => (v :> {"x": narrow})
