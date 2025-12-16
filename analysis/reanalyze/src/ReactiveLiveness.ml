(** Reactive liveness computation using fixpoint.
    
    Computes the set of live declarations by:
    1. Starting from roots (annotated + externally referenced)
    2. Propagating through references via fixpoint
    
    Uses pure reactive combinators - no internal hashtables. *)

type t = {
  live: (Lexing.position, unit) Reactive.t;
  edges: (Lexing.position, Lexing.position list) Reactive.t;
  roots: (Lexing.position, unit) Reactive.t;
}

(** Compute reactive liveness from ReactiveMerge.t *)
let create ~(merged : ReactiveMerge.t) : t =
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
     
     We use join to explicitly track the dependency on decls. When a decl at
     position P arrives, any ref with posFrom=P will be reprocessed. *)
  let external_value_refs : (Lexing.position, unit) Reactive.t =
    Reactive.join value_refs_from decls
      ~key_of:(fun posFrom _targets -> posFrom)
      ~f:(fun _posFrom targets decl_opt ->
        match decl_opt with
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
    Reactive.join type_refs_from decls
      ~key_of:(fun posFrom _targets -> posFrom)
      ~f:(fun _posFrom targets decl_opt ->
        match decl_opt with
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
  let live = Reactive.fixpoint ~init:all_roots ~edges () in
  {live; edges; roots = all_roots}

(** Print reactive collection update statistics *)
let print_stats ~(t : t) : unit =
  let print name (c : _ Reactive.t) =
    let s = Reactive.stats c in
    Printf.eprintf "  %s: recv=%d emit=%d len=%d\n" name s.updates_received
      s.updates_emitted (Reactive.length c)
  in
  Printf.eprintf "ReactiveLiveness collection stats:\n";
  print "roots" t.roots;
  print "edges" t.edges;
  print "live (fixpoint)" t.live
