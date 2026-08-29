/* Accepted object-field mutability and subtyping cases. Rejecting
 counterparts are under tests/build_tests/super_errors/fixtures/object_*.res. */

type wide = {"a": int, "b": int}
type narrow = {"a": int} /* wide <: narrow (width subtyping) */

/* Closed rows: coercion may forget write capability, covariantly.
 (Mutable A :> Immutable B with A <: B.) */
let forget_write_covariant = (v: {@set "x": wide}): {"x": narrow} => (v :> {"x": narrow})

/* Open source, closed immutable target: ordinary covariance. The result is
 read-only, while later source promotion writes at the source field type. */
let open_source_covariant = (o: {.."x": wide}): {"x": narrow} => (o :> {"x": narrow})

/* Closed source, open target: covariant; the target's tail is instantiated
 from the (closed) source, so the result is not promotable. */
let closed_source_open_target = (v: {"x": wide}) => (v :> {.."x": narrow})

/* Open source, mutable target at the same type: promotion constrains callers
 to objects with a mutable field. */
let open_source_promote_same_type = (o: {.."x": wide}): {@set "x": wide} => (o :> {@set "x": wide})

/* Writing a field of an open row promotes it and strengthens the demand on
 callers. */
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

/* Private-row signature inclusion may forget write capability: the
   implementation's settable field is abstracted to a read-only one. The
   reverse (a signature granting @set over a plain implementation field) is
   pinned as an error in
   tests/build_tests/super_errors/fixtures/object_private_row_grants_set.res.
   Writing a private row that already has @set is accepted; writing a
   private readonly row is pinned as an error in
   object_private_row_write.res. */
module PrivateRowForgetsSet: {
  type t = private {.."x": int}
} = {
  type t = private {..@set "x": int}
}

type private_settable = private {..@set "x": int}
let write_private_settable = (o: private_settable) => o["x"] = 1
