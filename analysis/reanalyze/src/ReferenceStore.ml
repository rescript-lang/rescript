(** Abstraction over reference storage.

    Allows the solver to work with either:
    - [Frozen]: Traditional [References.t] (copied from reactive)
    - [Reactive]: Direct reactive collections (no copy, zero-cost on warm runs)

    This eliminates the O(N) freeze step when using reactive mode. *)

type t =
  | Frozen of References.t
  | Reactive of {
      value_refs: (Lexing.position, PosSet.t) Reactive.t;
      type_refs: (Lexing.position, PosSet.t) Reactive.t;
      (* Type deps sources *)
      same_path_refs: (Lexing.position, PosSet.t) Reactive.t;
      cross_file_refs: (Lexing.position, PosSet.t) Reactive.t;
      impl_to_intf_refs_path2: (Lexing.position, PosSet.t) Reactive.t;
      intf_to_impl_refs: (Lexing.position, PosSet.t) Reactive.t;
      (* Exception refs source *)
      exception_resolved_refs: (Lexing.position, PosSet.t) Reactive.t;
    }

let of_frozen refs = Frozen refs

let of_reactive ~value_refs ~type_refs ~type_deps ~exception_refs =
  Reactive
    {
      value_refs;
      type_refs;
      same_path_refs = type_deps.ReactiveTypeDeps.same_path_refs;
      cross_file_refs = type_deps.ReactiveTypeDeps.cross_file_refs;
      impl_to_intf_refs_path2 =
        type_deps.ReactiveTypeDeps.impl_to_intf_refs_path2;
      intf_to_impl_refs = type_deps.ReactiveTypeDeps.intf_to_impl_refs;
      exception_resolved_refs =
        exception_refs.ReactiveExceptionRefs.resolved_refs;
    }

(** Helper to get from reactive and default to empty *)
let get_or_empty reactive pos =
  match Reactive.get reactive pos with
  | Some s -> s
  | None -> PosSet.empty

let find_value_refs t pos =
  match t with
  | Frozen refs -> References.find_value_refs refs pos
  | Reactive r ->
    (* Combine: per-file value_refs + exception resolved_refs *)
    let from_file = get_or_empty r.value_refs pos in
    let from_exceptions = get_or_empty r.exception_resolved_refs pos in
    PosSet.union from_file from_exceptions

let find_type_refs t pos =
  match t with
  | Frozen refs -> References.find_type_refs refs pos
  | Reactive r ->
    (* Combine: per-file type_refs + all type_deps sources *)
    let from_file = get_or_empty r.type_refs pos in
    let from_same_path = get_or_empty r.same_path_refs pos in
    let from_cross_file = get_or_empty r.cross_file_refs pos in
    let from_impl_intf2 = get_or_empty r.impl_to_intf_refs_path2 pos in
    let from_intf_impl = get_or_empty r.intf_to_impl_refs pos in
    from_file
    |> PosSet.union from_same_path
    |> PosSet.union from_cross_file
    |> PosSet.union from_impl_intf2
    |> PosSet.union from_intf_impl

(** Get underlying References.t for Frozen stores. Used for forward liveness. *)
let get_refs_opt t =
  match t with
  | Frozen refs -> Some refs
  | Reactive _ -> None
