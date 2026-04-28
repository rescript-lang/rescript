(** Forward liveness fixpoint computation.

    Computes the set of live declarations by forward propagation:
    1. Start with roots (inherently live declarations)
    2. For each live declaration, mark what it references as live
    3. Repeat until fixpoint
    
    Roots include:
    - Declarations annotated @live or @genType
    - Declarations referenced from non-declaration positions (external uses)
    
    Note: refs_from is keyed by expression positions, not declaration positions.
    We need to find all refs where posFrom is within the declaration's range. *)

(** Reason why a declaration is live *)
type live_reason =
  | Annotated  (** Has @live or @genType annotation *)
  | ExternalRef  (** Referenced from outside any declaration *)
  | Propagated  (** Referenced by another live declaration *)

let reason_to_string = function
  | Annotated -> "annotated"
  | ExternalRef -> "external ref"
  | Propagated -> "propagated"

(** Check if a position is within a declaration's range *)
let pos_in_decl (pos : Lexing.position) (decl : Decl.t) : bool =
  pos.pos_fname = decl.pos.pos_fname
  && pos.pos_cnum >= decl.posStart.pos_cnum
  && pos.pos_cnum <= decl.posEnd.pos_cnum

(** Build a hashtable mapping posTo -> bool indicating if it has external refs.
    External refs are refs where posFrom is NOT a declaration position.
    (Matching backward algorithm: it checks find_opt, not range containment) *)
let find_externally_referenced ~(decl_store : DeclarationStore.t)
    ~(refs : References.t) : bool PosHash.t =
  let externally_referenced = PosHash.create 256 in

  (* Helper: check if posFrom is a declaration position *)
  let is_decl_pos posFrom =
    DeclarationStore.find_opt decl_store posFrom <> None
  in

  (* Check value refs *)
  References.iter_value_refs_from refs (fun posFrom posToSet ->
      if not (is_decl_pos posFrom) then
        PosSet.iter
          (fun posTo -> PosHash.replace externally_referenced posTo true)
          posToSet);

  (* Check type refs *)
  References.iter_type_refs_from refs (fun posFrom posToSet ->
      if not (is_decl_pos posFrom) then
        PosSet.iter
          (fun posTo -> PosHash.replace externally_referenced posTo true)
          posToSet);

  externally_referenced

(** Check if a declaration is inherently live (a root) *)
let is_root ~ann_store ~externally_referenced (decl : Decl.t) =
  AnnotationStore.is_annotated_gentype_or_live ann_store decl.pos
  || PosHash.mem externally_referenced decl.pos

(** Build index mapping declaration positions to their outgoing refs.
    Done once upfront to avoid O(worklist × refs) in the main loop.
    
    Optimized by grouping declarations by file first, so we only check
    declarations in the same file as each ref source. *)
let build_decl_refs_index ~(decl_store : DeclarationStore.t)
    ~(refs : References.t) : (PosSet.t * PosSet.t) PosHash.t =
  let index = PosHash.create 256 in

  (* Group declarations by file for efficient lookup *)
  let decls_by_file : (string, (Lexing.position * Decl.t) list) Hashtbl.t =
    Hashtbl.create 256
  in
  DeclarationStore.iter
    (fun pos decl ->
      let fname = pos.Lexing.pos_fname in
      let existing =
        try Hashtbl.find decls_by_file fname with Not_found -> []
      in
      Hashtbl.replace decls_by_file fname ((pos, decl) :: existing))
    decl_store;

  (* Helper to add targets to a declaration's index entry *)
  let add_targets decl_pos targets ~is_type =
    let value_targets, type_targets =
      match PosHash.find_opt index decl_pos with
      | Some pair -> pair
      | None -> (PosSet.empty, PosSet.empty)
    in
    let new_pair =
      if is_type then (value_targets, PosSet.union type_targets targets)
      else (PosSet.union value_targets targets, type_targets)
    in
    PosHash.replace index decl_pos new_pair
  in

  (* For each ref, find which declaration (in same file) contains its source *)
  let process_ref posFrom posToSet ~is_type =
    let fname = posFrom.Lexing.pos_fname in
    match Hashtbl.find_opt decls_by_file fname with
    | None -> () (* No declarations in this file *)
    | Some decls_in_file ->
      List.iter
        (fun (decl_pos, decl) ->
          if pos_in_decl posFrom decl then
            add_targets decl_pos posToSet ~is_type)
        decls_in_file
  in

  References.iter_value_refs_from refs (fun posFrom posToSet ->
      process_ref posFrom posToSet ~is_type:false);
  References.iter_type_refs_from refs (fun posFrom posToSet ->
      process_ref posFrom posToSet ~is_type:true);

  index

(** Compute liveness using forward propagation from roots.
    Returns a hashtable mapping positions to their live reason. *)
let compute_forward ~debug ~(decl_store : DeclarationStore.t)
    ~(refs : References.t) ~(ann_store : AnnotationStore.t) :
    live_reason PosHash.t * (PosSet.t * PosSet.t) PosHash.t =
  let t0 = Unix.gettimeofday () in
  let live = PosHash.create 256 in
  let worklist = Queue.create () in
  let root_count = ref 0 in
  let propagated_count = ref 0 in

  (* Find declarations with external references *)
  let externally_referenced = find_externally_referenced ~decl_store ~refs in

  (* Pre-compute index: decl_pos -> (value_targets, type_targets) *)
  let decl_refs_index = build_decl_refs_index ~decl_store ~refs in

  if debug then (
    (* Compute some high-level stats about the dependency graph. Note: this is
       declaration-to-declaration deps only (after mapping ref posFrom into the
       containing declaration). *)
    let decls_with_out = ref 0 in
    let out_edges_to_decls = ref 0 in
    PosHash.iter
      (fun _decl_pos (value_targets, type_targets) ->
        incr decls_with_out;
        let count_targets targets =
          PosSet.fold
            (fun target acc ->
              match DeclarationStore.find_opt decl_store target with
              | Some _ -> acc + 1
              | None -> acc)
            targets 0
        in
        out_edges_to_decls :=
          !out_edges_to_decls
          + count_targets value_targets
          + count_targets type_targets)
      decl_refs_index;
    Log_.item "@.Forward Liveness Analysis@.@.";
    Log_.item "  decls: %d@."
      (DeclarationStore.fold (fun _ _ acc -> acc + 1) decl_store 0);
    Log_.item "  roots(external targets): %d@."
      (PosHash.length externally_referenced);
    Log_.item "  decl-deps: decls_with_out=%d edges_to_decls=%d@.@."
      !decls_with_out !out_edges_to_decls);

  (* Initialize with roots *)
  DeclarationStore.iter
    (fun pos decl ->
      if is_root ~ann_store ~externally_referenced decl then (
        incr root_count;
        let reason =
          if AnnotationStore.is_annotated_gentype_or_live ann_store pos then
            Annotated
          else ExternalRef
        in
        PosHash.replace live pos reason;
        Queue.push (pos, decl) worklist;
        if debug then
          Log_.item "  Root (%s): %s %s@." (reason_to_string reason)
            (decl.declKind |> Decl.Kind.toString)
            (decl.path |> DcePath.toString)))
    decl_store;

  if debug then Log_.item "@.  %d roots found@.@." !root_count;

  (* Forward propagation fixpoint.
     For each live declaration, look up its outgoing refs from the index. *)
  while not (Queue.is_empty worklist) do
    let pos, decl = Queue.pop worklist in

    (* Skip if this position is annotated @dead - don't propagate from it *)
    if not (AnnotationStore.is_annotated_dead ann_store pos) then
      (* Look up pre-computed targets for this declaration *)
      match PosHash.find_opt decl_refs_index pos with
      | None -> () (* No outgoing refs from this declaration *)
      | Some (value_targets, type_targets) ->
        (* Propagate to value targets that are value declarations *)
        PosSet.iter
          (fun target ->
            if not (PosHash.mem live target) then
              match DeclarationStore.find_opt decl_store target with
              | Some target_decl
                when not (target_decl.declKind |> Decl.Kind.isType) ->
                incr propagated_count;
                PosHash.replace live target Propagated;
                Queue.push (target, target_decl) worklist;
                if debug then
                  Log_.item "  Propagate: %s -> %s@."
                    (decl.path |> DcePath.toString)
                    (target_decl.path |> DcePath.toString)
              | Some _ ->
                (* Type target from value ref - see below *)
                ()
              | None ->
                (* External or non-declaration target *)
                PosHash.replace live target Propagated)
          value_targets;

        (* Propagate to type targets that are type declarations *)
        PosSet.iter
          (fun target ->
            if not (PosHash.mem live target) then
              match DeclarationStore.find_opt decl_store target with
              | Some target_decl when target_decl.declKind |> Decl.Kind.isType
                ->
                incr propagated_count;
                PosHash.replace live target Propagated;
                Queue.push (target, target_decl) worklist;
                if debug then
                  Log_.item "  Propagate: %s -> %s@."
                    (decl.path |> DcePath.toString)
                    (target_decl.path |> DcePath.toString)
              | Some _ ->
                (* Value target from type ref - skip *)
                ()
              | None ->
                (* External or non-declaration target *)
                PosHash.replace live target Propagated)
          type_targets
  done;

  if debug then
    Log_.item "@.  %d declarations marked live via propagation@.@."
      !propagated_count;

  let t1 = Unix.gettimeofday () in
  if !Cli.timing then
    Printf.eprintf
      "  Liveness.compute_forward: %.3fms (roots=%d, propagated=%d, live=%d)\n\
       %!"
      ((t1 -. t0) *. 1000.0)
      !root_count !propagated_count (PosHash.length live);

  (live, decl_refs_index)

(** Check if a position is live according to forward-computed liveness *)
let is_live_forward ~(live : live_reason PosHash.t) (pos : Lexing.position) :
    bool =
  PosHash.mem live pos

(** Get the reason why a position is live, if it is *)
let get_live_reason ~(live : live_reason PosHash.t) (pos : Lexing.position) :
    live_reason option =
  PosHash.find_opt live pos
