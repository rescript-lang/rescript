/* Pins the current typing behavior of object-field mutability (encoded today
   as phantom `"x#="` setter members) ahead of the representation cleanup
   described in docs/object_representation_cleanup.md.

   Every case in this file compiles today. The ones marked EXPECTED TO FLIP
   are intentionally rejected by the new model (single storage location: a
   field has one type; promotion only adds write capability, it never
   changes the type). The others must keep compiling unchanged.

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

/* EXPECTED TO FLIP: today an open row can acquire a setter at a DIFFERENT
   type than its getter, because writability is a separate member — this
   coercion leaves getter type `wide` and setter type `narrow` on one
   property. The new model is capability-only (Immutable A -> Mutable A,
   then A = B required), so this becomes a compile error when the cleanup's
   Stage D lands. */
let open_source_setter_narrower = (o: {.."x": wide}): {@set "x": narrow} =>
  (o :> {@set "x": narrow})

/* EXPECTED TO FLIP (unsoundness, the strongest case).

   Today this whole block compiles, and `run_unrelated_setter()` returns
   "hello" at declared type `int` when `plain_int_obj` is the plain JS
   object {x: 1}.

   Mechanism: writability is a separate row member, so the assignment mints
   "x#=": string => unit in o's open row from the right-hand side's type,
   never relating it to the getter "x": int. The inferred demand is
   {.."x": int, "x#=": string => unit} — but both members compile to the
   same storage `o.x`, so the write invalidates the getter's type.

   New model: the assignment promotes the field to `Mutable int`, and
   assigning a `string` is a unification error — the flip enforces the
   getter/setter consistency invariant that is missing today. */
let unrelated_setter_type = (o: {.."x": int}): int => {
  o["x"] = "hello"
  o["x"]
}

@val external plain_int_obj: {.."x": int} = "plainIntObj"
let run_unrelated_setter = (): int => unrelated_setter_type(plain_int_obj)
