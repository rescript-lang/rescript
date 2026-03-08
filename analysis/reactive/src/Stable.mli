(** Values marked for storage in stable (C-allocated) containers.

    Stable containers live outside the OCaml GC heap. This means the GC will
    not trace or move their contents. Two consequences:

    {b Storing values.} Only values that are {e not} in the minor heap may be
    stored. Use [of_value] (checked) or [unsafe_of_value] (unchecked) to mark
    a value before storing it. [unsafe_of_value] is the only truly unsafe
    operation: if a minor-heap value is stored, the GC may relocate the
    original and the stable container will hold a dangling pointer.

    {b Reading values.} Use [to_linear_value] to read a value back. The
    result is an ordinary OCaml value that is {e not} protected by the GC
    (the stable container owns the only reference). The caller must consume
    it immediately and not stash it in a long-lived OCaml data structure,
    because the stable container may destroy or overwrite its slot at any
    time. Short-lived uses (comparison, passing to a function, computing a
    result) are fine.

    {b Stable-safety.} A module is {e stable-safe} when it contains zero calls
    to [unsafe_of_value] — all stored values are known stable by construction.
    See [STABLE_SAFETY.md] in the reactive directory for a guide on how to
    establish stable-safety and repair violations. *)

type 'a t

val unsafe_of_value : 'a -> 'a t
(** Unsafely mark a value as suitable for stable storage. The caller must
    ensure the value is not in the minor heap.
    This is the only truly unsafe operation in the module. *)

val of_value : 'a -> 'a t
(** Safely mark a value as suitable for stable storage.

    Raises [Invalid_argument] if the value is currently in the minor heap.
    Immediates are accepted. *)

val int : int -> int t
(** Safely mark an [int] as suitable for stable storage. *)

val unit : unit t
(** [()] as a stable value. *)

val to_linear_value : 'a t -> 'a
(** Read a value from a stable container. The result must be consumed
    immediately (linear use) and not stored in long-lived OCaml structures,
    as the stable container may destroy or overwrite the slot at any time. *)

val unsafe_to_nonlinear_value : 'a t -> 'a
(** Like [to_linear_value] but explicitly marks a non-linear use: the
    returned value will be stored in a long-lived OCaml structure (e.g.,
    a hashtable or accumulator list). This is safe only when the stable
    container will not destroy or overwrite the slot while the OCaml
    reference is alive. Each call site should be audited individually. *)
