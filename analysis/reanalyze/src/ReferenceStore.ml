(** Abstraction over reference storage.

    Allows the solver to work with either:
    - [Frozen]: Traditional [References.t] (copied from reactive)
    - [Reactive]: Direct reactive collections (no copy, zero-cost on warm runs)

    This eliminates the O(N) freeze step when using reactive mode. *)

type t =
  | Frozen of References.t
  | Reactive of {
      (* Per-file refs_from *)
      value_refs_from: (Lexing.position, PosSet.t) Reactive.t;
      type_refs_from: (Lexing.position, PosSet.t) Reactive.t;
      (* Type deps refs_from *)
      all_type_refs_from: (Lexing.position, PosSet.t) Reactive.t;
      (* Exception refs_from *)
      exception_value_refs_from: (Lexing.position, PosSet.t) Reactive.t;
    }

let of_frozen refs = Frozen refs

let of_reactive ~value_refs_from ~type_refs_from ~type_deps ~exception_refs =
  Reactive
    {
      value_refs_from;
      type_refs_from;
      all_type_refs_from = type_deps.ReactiveTypeDeps.all_type_refs_from;
      exception_value_refs_from =
        exception_refs.ReactiveExceptionRefs.resolved_refs_from;
    }

(** Get underlying References.t for Frozen stores. Used for forward liveness. *)
let get_refs_opt t =
  match t with
  | Frozen refs -> Some refs
  | Reactive _ -> None
