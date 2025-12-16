(** Reactive liveness computation using fixpoint.
    
    Computes the set of live declarations by:
    1. Starting from roots (annotated + externally referenced)
    2. Propagating through references via fixpoint
    
    Uses pure reactive combinators - no internal hashtables. *)

(** Compute reactive liveness from ReactiveMerge.t *)
let create ~(merged : ReactiveMerge.t) : (Lexing.position, unit) Reactive.t =
  let decls = merged.decls in
  let annotations = merged.annotations in

  (* Combine value refs using union: per-file refs + exception refs *)
  let value_refs_from : (Lexing.position, PosSet.t) Reactive.t =
    Reactive.union merged.value_refs_from
      merged.exception_refs.resolved_refs_from ~merge:PosSet.union ()
  in

  (* Combine type refs using union: per-file refs + type deps from ReactiveTypeDeps *)
  let type_refs_from : (Lexing.position, PosSet.t) Reactive.t =
    Reactive.union merged.type_refs_from merged.type_deps.all_type_refs_from
      ~merge:PosSet.union ()
  in

  (* Step 1: Build decl_refs_index - maps decl -> (value_targets, type_targets) *)
  let decl_refs_index =
    ReactiveDeclRefs.create ~decls ~value_refs_from ~type_refs_from
  in

  (* Step 2: Convert to edges format for fixpoint: decl -> successor list *)
  let edges : (Lexing.position, Lexing.position list) Reactive.t =
    Reactive.flatMap decl_refs_index
      ~f:(fun pos (value_targets, type_targets) ->
        let all_targets = PosSet.union value_targets type_targets in
        [(pos, PosSet.elements all_targets)])
      ()
  in

  (* Step 3: Compute roots - positions that are inherently live *)
  (* Root if: annotated @live/@genType OR referenced from outside any decl *)

  (* Compute externally referenced positions reactively.
     A position is externally referenced if any reference to it comes from
     a position that is NOT a declaration position (exact match).
     
     This matches the non-reactive algorithm which uses DeclarationStore.find_opt.
     
     We use flatMap and check decls synchronously within the function.
     This works correctly regardless of delta arrival order because the check
     happens at evaluation time when decls has current data. *)
  (* Compute externally referenced positions reactively.
     A position is externally referenced if any reference to it comes from
     a position that is NOT a declaration position (exact match).
     
     This matches the non-reactive algorithm which uses DeclarationStore.find_opt.
     
     We use flatMap and check decls synchronously within the function.
     This works correctly regardless of delta arrival order because the check
     happens at evaluation time when decls has current data. *)
  let external_value_refs : (Lexing.position, unit) Reactive.t =
    Reactive.flatMap value_refs_from
      ~f:(fun posFrom targets ->
        match decls.get posFrom with
        | Some _ ->
          (* posFrom IS a decl position, refs are internal *)
          []
        | None ->
          (* posFrom is NOT a decl position, targets are externally referenced *)
          PosSet.elements targets |> List.map (fun posTo -> (posTo, ())))
      ~merge:(fun () () -> ())
      ()
  in

  let external_type_refs : (Lexing.position, unit) Reactive.t =
    Reactive.flatMap type_refs_from
      ~f:(fun posFrom targets ->
        match decls.get posFrom with
        | Some _ ->
          (* posFrom IS a decl position, refs are internal *)
          []
        | None ->
          (* posFrom is NOT a decl position, targets are externally referenced *)
          PosSet.elements targets |> List.map (fun posTo -> (posTo, ())))
      ~merge:(fun () () -> ())
      ()
  in

  let externally_referenced : (Lexing.position, unit) Reactive.t =
    Reactive.union external_value_refs external_type_refs
      ~merge:(fun () () -> ())
      ()
  in

  (* Compute annotated roots: decls with @live or @genType *)
  let annotated_roots : (Lexing.position, unit) Reactive.t =
    Reactive.join decls annotations
      ~key_of:(fun pos _decl -> pos)
      ~f:(fun pos _decl ann_opt ->
        match ann_opt with
        | Some FileAnnotations.Live | Some FileAnnotations.GenType ->
          [(pos, ())]
        | _ -> [])
      ~merge:(fun () () -> ())
      ()
  in

  (* Combine all roots *)
  let all_roots : (Lexing.position, unit) Reactive.t =
    Reactive.union annotated_roots externally_referenced
      ~merge:(fun () () -> ())
      ()
  in

  (* Step 4: Compute fixpoint - all reachable positions from roots *)
  Reactive.fixpoint ~init:all_roots ~edges ()
