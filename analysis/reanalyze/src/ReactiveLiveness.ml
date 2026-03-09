(** Reactive liveness computation using fixpoint.
    
    Computes the set of live declarations by:
    1. Starting from roots (annotated + externally referenced)
    2. Propagating through references via fixpoint
    
    Uses pure reactive combinators - no internal hashtables. *)

type t = {
  live: (Lexing.position, unit) Reactive.t;
  edges: (Lexing.position, Lexing.position StableList.t) Reactive.t;
  roots: (Lexing.position, unit) Reactive.t;
}

(** Compute reactive liveness from ReactiveMerge.t *)
let create ~(merged : ReactiveMerge.t) : t =
  let decls = merged.decls in
  let annotations = merged.annotations in

  (* Combine value refs using union: per-file refs + exception refs *)
  let value_refs_from : (Lexing.position, PosSet.t) Reactive.t =
    Reactive.Union.create ~name:"liveness.value_refs_from"
      merged.value_refs_from merged.exception_refs.resolved_refs_from
      ~merge:(fun a b ->
        Stable.unsafe_of_value
          (PosSet.union (Stable.to_linear_value a) (Stable.to_linear_value b)))
      ()
  in

  (* Combine type refs using union: per-file refs + type deps from ReactiveTypeDeps *)
  let type_refs_from : (Lexing.position, PosSet.t) Reactive.t =
    Reactive.Union.create ~name:"liveness.type_refs_from" merged.type_refs_from
      merged.type_deps.all_type_refs_from
      ~merge:(fun a b ->
        Stable.unsafe_of_value
          (PosSet.union (Stable.to_linear_value a) (Stable.to_linear_value b)))
      ()
  in

  (* Step 1: Build decl_refs_index - maps decl -> (value_targets, type_targets) *)
  let decl_refs_index =
    ReactiveDeclRefs.create ~decls ~value_refs_from ~type_refs_from
  in

  (* Step 2: Convert to edges format for fixpoint: decl -> successor list *)
  let edges : (Lexing.position, Lexing.position StableList.t) Reactive.t =
    Reactive.FlatMap.create ~name:"liveness.edges" decl_refs_index
      ~f:(fun pos v wave ->
        let pos = Stable.to_linear_value pos in
        let value_targets, type_targets = Stable.to_linear_value v in
        let all_targets = PosSet.union value_targets type_targets in
        StableWave.push wave
          (Stable.unsafe_of_value pos)
          (Stable.unsafe_of_value
             (StableList.unsafe_of_list (PosSet.elements all_targets))))
      ()
  in

  (* Step 3: Compute roots - positions that are inherently live *)
  (* Root if: annotated @live/@genType OR referenced from outside any decl *)

  (* Compute externally referenced positions reactively.
     A position is externally referenced if any reference to it comes from
     a position that is NOT a declaration position (exact match).
     
     This matches the non-reactive algorithm which uses DeclarationStore.find_opt.
     
     We use join to explicitly track the dependency on decls. When a decl at
     position P arrives, any ref with posFrom=P will be reprocessed. *)
  let external_value_refs : (Lexing.position, unit) Reactive.t =
    Reactive.Join.create ~name:"liveness.external_value_refs" value_refs_from
      decls
      ~key_of:(fun posFrom _targets -> posFrom)
      ~f:(fun _posFrom targets decl_mb wave ->
        let targets = Stable.to_linear_value targets in
        if not (Maybe.is_some decl_mb) then
          (* posFrom is NOT a decl position, targets are externally referenced *)
          PosSet.elements targets
          |> List.iter (fun posTo ->
                 StableWave.push wave (Stable.unsafe_of_value posTo) Stable.unit))
      ~merge:(fun _ _ -> Stable.unit)
      ()
  in

  let external_type_refs : (Lexing.position, unit) Reactive.t =
    Reactive.Join.create ~name:"liveness.external_type_refs" type_refs_from
      decls
      ~key_of:(fun posFrom _targets -> posFrom)
      ~f:(fun _posFrom targets decl_mb wave ->
        let targets = Stable.to_linear_value targets in
        if not (Maybe.is_some decl_mb) then
          (* posFrom is NOT a decl position, targets are externally referenced *)
          PosSet.elements targets
          |> List.iter (fun posTo ->
                 StableWave.push wave (Stable.unsafe_of_value posTo) Stable.unit))
      ~merge:(fun _ _ -> Stable.unit)
      ()
  in

  let externally_referenced : (Lexing.position, unit) Reactive.t =
    Reactive.Union.create ~name:"liveness.externally_referenced"
      external_value_refs external_type_refs
      ~merge:(fun _ _ -> Stable.unit)
      ()
  in

  (* Compute annotated roots: decls with @live or @genType *)
  let annotated_roots : (Lexing.position, unit) Reactive.t =
    Reactive.Join.create ~name:"liveness.annotated_roots" decls annotations
      ~key_of:(fun pos _decl -> pos)
      ~f:(fun pos _decl ann_mb wave ->
        let pos = Stable.to_linear_value pos in
        if Maybe.is_some ann_mb then
          match Stable.to_linear_value (Maybe.unsafe_get ann_mb) with
          | FileAnnotations.Live | FileAnnotations.GenType ->
            StableWave.push wave (Stable.unsafe_of_value pos) Stable.unit
          | _ -> ())
      ~merge:(fun _ _ -> Stable.unit)
      ()
  in

  (* Combine all roots *)
  let all_roots : (Lexing.position, unit) Reactive.t =
    Reactive.Union.create ~name:"liveness.all_roots" annotated_roots
      externally_referenced
      ~merge:(fun _ _ -> Stable.unit)
      ()
  in

  (* Step 4: Compute fixpoint - all reachable positions from roots *)
  let live =
    Reactive.Fixpoint.create ~name:"liveness.live" ~init:all_roots ~edges ()
  in
  {live; edges; roots = all_roots}

(** Print reactive collection update statistics *)
let print_stats ~(t : t) : unit =
  let print name (c : _ Reactive.t) =
    let s = Reactive.stats c in
    Printf.eprintf
      "  %s: recv=%d/%d +%d -%d | emit=%d/%d +%d -%d | runs=%d len=%d\n" name
      s.deltas_received s.entries_received s.adds_received s.removes_received
      s.deltas_emitted s.entries_emitted s.adds_emitted s.removes_emitted
      s.process_count (Reactive.length c)
  in
  Printf.eprintf "ReactiveLiveness stats (recv=d/e/+/- emit=d/e/+/- runs):\n";
  print "roots" t.roots;
  print "edges" t.edges;
  print "live (fixpoint)" t.live
