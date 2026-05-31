module FileContext = struct
  type t = {source_path: string; module_name: string; is_interface: bool}

  (** Get module name as Name.t tagged with interface/implementation info *)
  let module_name_tagged file =
    file.module_name |> Name.create ~is_interface:file.is_interface

  let is_interface (file : t) = file.is_interface
end

(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

module Config = struct
  (* Turn on type analysis *)
  let analyze_types = ref true
  let analyze_externals = ref false
  let report_underscore = false
  let report_types_dead_only_in_interface = false
  let warn_on_circular_dependencies = false
end

let rec check_sub s1 s2 n =
  n <= 0
  || (try s1.[n] = s2.[n] with Invalid_argument _ -> false)
     && check_sub s1 s2 (n - 1)

let file_is_implementation_of s1 s2 =
  let n1 = String.length s1 and n2 = String.length s2 in
  n2 = n1 + 1 && check_sub s1 s2 (n1 - 1)

let live_annotation = "live"

type decls = Decl.t PosHash.t
(** type alias for declaration hashtables *)

(* NOTE: Global decls removed - now using Declarations.builder/t pattern *)

(* NOTE: Global ValueReferences removed - now using References.builder/t pattern *)

(* Local reporting context used only while emitting dead-code warnings.
   It tracks, per file, the end position of the last value we reported on,
   so nested values inside that range don't get duplicate warnings. *)
module ReportingContext = struct
  type t = Lexing.position ref

  let create () : t = ref Lexing.dummy_pos
  let get_max_end (ctx : t) = !ctx
  let set_max_end (ctx : t) (pos : Lexing.position) = ctx := pos
end

(* NOTE: Global TypeReferences removed - now using References.builder/t pattern *)

let decl_get_loc decl =
  let loc_start =
    let offset =
      match decl.Decl.pos_adjustment with
      | FirstVariant | Nothing -> 0
      | OtherVariant -> 2
    in
    let cnum_with_offset = decl.pos_start.pos_cnum + offset in
    if cnum_with_offset < decl.pos_end.pos_cnum then
      {decl.pos_start with pos_cnum = cnum_with_offset}
    else decl.pos_start
  in
  {Location.loc_start; loc_end = decl.pos_end; loc_ghost = false}

let add_value_reference ~config ~refs ~file_deps ~(binding : Location.t)
    ~add_file_reference ~(loc_from : Location.t) ~(loc_to : Location.t) : unit =
  let effective_from = if binding = Location.none then loc_from else binding in
  if not effective_from.loc_ghost then (
    if config.DceConfig.cli.debug then
      Log_.item "addValueReference %s --> %s@."
        (effective_from.loc_start |> Pos.to_string)
        (loc_to.loc_start |> Pos.to_string);
    References.add_value_ref refs ~pos_to:loc_to.loc_start
      ~pos_from:effective_from.loc_start;
    if
      add_file_reference && (not loc_to.loc_ghost)
      && (not effective_from.loc_ghost)
      && effective_from.loc_start.pos_fname <> loc_to.loc_start.pos_fname
    then
      FileDeps.add_dep file_deps ~from_file:effective_from.loc_start.pos_fname
        ~to_file:loc_to.loc_start.pos_fname)

let addDeclaration_ ~config ~decls ~(file : FileContext.t) ?pos_end ?pos_start
    ~decl_kind ~path ~(loc : Location.t) ?(pos_adjustment = Decl.Nothing)
    ?manifest_type_path ~module_loc (name : Name.t) =
  let pos = loc.loc_start in
  let pos_start =
    match pos_start with
    | Some pos_start -> pos_start
    | None -> pos
  in
  let pos_end =
    match pos_end with
    | Some pos_end -> pos_end
    | None -> loc.loc_end
  in
  (* a .cmi file can contain locations from other files.
     For instance:
         module M : Set.S with type elt = int
     will create value definitions whose location is in set.mli
  *)
  if (not loc.loc_ghost) && pos.pos_fname = file.source_path then (
    if config.DceConfig.cli.debug then
      Log_.item "add%sDeclaration %s %s path:%s@."
        (decl_kind |> Decl.Kind.to_string)
        (name |> Name.to_string) (pos |> Pos.to_string) (path |> DcePath.to_string);
    let decl =
      {
        Decl.decl_kind;
        module_loc;
        pos_adjustment;
        path = name :: path;
        manifest_type_path;
        pos;
        pos_end;
        pos_start;
        resolved_dead = None;
        report = true;
      }
    in
    Declarations.add decls pos decl)

let add_value_declaration ~config ~decls ~file ?(is_toplevel = true)
    ~(loc : Location.t) ~module_loc ?(optional_args = OptionalArgs.empty) ~path
    ~side_effects name =
  name
  |> addDeclaration_ ~config ~decls ~file
       ~decl_kind:(Value {is_toplevel; optional_args; side_effects})
       ~loc ~module_loc ~path

(** Create a dead code issue. Pure - no side effects. *)
let make_dead_issue ~decl ~message dead_warning : Issue.t =
  let loc = decl |> decl_get_loc in
  AnalysisResult.make_dead_issue ~loc ~dead_warning
    ~path:(DcePath.without_head decl.path)
    ~message

let is_inside_reported_value (ctx : ReportingContext.t) decl =
  let max_end = ReportingContext.get_max_end ctx in
  let file_has_changed = max_end.pos_fname <> decl.Decl.pos.pos_fname in
  let inside_reported_value =
    decl |> Decl.is_value && (not file_has_changed)
    && max_end.pos_cnum > decl.pos.pos_cnum
  in
  if not inside_reported_value then
    if decl |> Decl.is_value then
      if file_has_changed || decl.pos_end.pos_cnum > max_end.pos_cnum then
        ReportingContext.set_max_end ctx decl.pos_end;
  inside_reported_value

(** Check if a reference position is "below" the declaration.
    A ref is below if it's in a different file, or comes after the declaration
    (but not inside it, e.g. not a callback). *)
let ref_is_below (decl : Decl.t) (pos_from : Lexing.position) =
  decl.pos.pos_fname <> pos_from.pos_fname
  || decl.pos.pos_cnum < pos_from.pos_cnum
     &&
     (* not a function defined inside a function, e.g. not a callback *)
     decl.pos_end.pos_cnum < pos_from.pos_cnum

(** Create hasRefBelow function using on-demand per-decl search.
    [iter_value_refs_from] iterates over (posFrom, posToSet) pairs.
    O(total_refs) per dead decl, but dead decls should be few. *)
let make_hasRefBelow ~transitive ~iter_value_refs_from =
  if transitive then fun _ -> false
  else fun decl ->
    let found = ref false in
    iter_value_refs_from (fun pos_from pos_to_set ->
        if (not !found) && PosSet.mem decl.Decl.pos pos_to_set then
          if ref_is_below decl pos_from then found := true);
    !found

(** Report a dead declaration. Returns list of issues (dead module first, then dead value).
    [hasRefBelow] checks if there are references from "below" the declaration.
    Only used when [config.run.transitive] is false.
    [?checkModuleDead] optional callback for checking dead modules. Defaults to DeadModules.checkModuleDead.
    [?shouldReport] optional callback to check if a decl should be reported. Defaults to checking decl.report. *)
let report_declaration ~config ~has_ref_below ?check_module_dead ?should_report
    (ctx : ReportingContext.t) decl : Issue.t list =
  let inside_reported_value = decl |> is_inside_reported_value ctx in
  let should_report =
    match should_report with
    | Some f -> f decl
    | None -> decl.report
  in
  (* For type re-exports (type y = x = {...}), the re-exported record/variant
     labels are restated but not independently actionable. Avoid duplicate/noisy
     warnings by suppressing reporting for the re-exported copy. *)
  let should_report =
    should_report
    &&
    match (decl.decl_kind, decl.manifest_type_path) with
    | (RecordLabel | VariantCase), Some _ -> false
    | _ -> true
  in
  if not should_report then []
  else
    let dead_warning, message =
      match decl.decl_kind with
      | Exception ->
        (Issue.WarningDeadException, "is never raised or passed as value")
      | Value {side_effects} -> (
        let no_side_effects_or_underscore =
          (not side_effects)
          ||
          match decl.path with
          | hd :: _ -> hd |> Name.starts_with_underscore
          | [] -> false
        in
        ( (match not no_side_effects_or_underscore with
          | true -> WarningDeadValueWithSideEffects
          | false -> WarningDeadValue),
          match decl.path with
          | name :: _ when name |> Name.is_underscore ->
            "has no side effects and can be removed"
          | _ -> (
            "is never used"
            ^
            match not no_side_effects_or_underscore with
            | true -> " and could have side effects"
            | false -> "") ))
      | RecordLabel ->
        (WarningDeadType, "is a record label never used to read a value")
      | VariantCase ->
        (WarningDeadType, "is a variant case which is never constructed")
    in
    let should_emit_warning =
      (not inside_reported_value)
      && (match decl.path with
         | name :: _ when name |> Name.is_underscore -> Config.report_underscore
         | _ -> true)
      && (config.DceConfig.run.transitive || not (has_ref_below decl))
    in
    if should_emit_warning then
      let module_name =
        decl.path
        |> DcePath.to_module_name ~is_type:(decl.decl_kind |> Decl.Kind.is_type)
      in
      let dead_module_issue =
        match check_module_dead with
        | Some f -> f ~file_name:decl.pos.pos_fname module_name
        | None ->
          DeadModules.check_module_dead ~config ~file_name:decl.pos.pos_fname
            module_name
      in
      let dead_value_issue = make_dead_issue ~decl ~message dead_warning in
      (* Return in order: dead module first (if any), then dead value *)
      match dead_module_issue with
      | Some mi -> [mi; dead_value_issue]
      | None -> [dead_value_issue]
    else []

let do_report_dead ~ann_store pos =
  not (AnnotationStore.is_annotated_gentype_or_dead ann_store pos)

(** Forward-based solver using refs_from direction.
    Computes liveness via forward propagation, then processes declarations. *)
let solve_dead_forward ~ann_store ~config ~decl_store ~refs ~optional_args_state
    ~check_optional_arg:
      (check_optional_arg_fn :
        optional_args_state:OptionalArgsState.t ->
        ann_store:AnnotationStore.t ->
        config:DceConfig.t ->
        Decl.t ->
        Issue.t list) : AnalysisResult.t =
  (* Compute liveness using forward propagation *)
  let debug = config.DceConfig.cli.debug in
  let transitive = config.DceConfig.run.transitive in
  let live, decl_refs_index =
    Liveness.compute_forward ~debug ~decl_store ~refs ~ann_store
  in

  (* For debug logging: invert decl_refs_index to get incoming deps between
     declarations. This is useful for understanding why something is dead
     ("who points to it?") even though the solver itself is forward. *)
  let incoming_decl_deps : PosSet.t PosHash.t =
    if not debug then PosHash.create 0
    else
      let incoming = PosHash.create 256 in
      let add_incoming ~target ~source =
        let existing =
          match PosHash.find_opt incoming target with
          | Some s -> s
          | None -> PosSet.empty
        in
        PosHash.replace incoming target (PosSet.add source existing)
      in
      PosHash.iter
        (fun source_pos (value_targets, type_targets) ->
          let add_targets targets =
            PosSet.iter
              (fun target_pos ->
                match DeclarationStore.find_opt decl_store target_pos with
                | Some _ -> add_incoming ~target:target_pos ~source:source_pos
                | None -> ())
              targets
          in
          add_targets value_targets;
          add_targets type_targets)
        decl_refs_index;
      incoming
  in

  (* hasRefBelow uses on-demand search through refs_from *)
  let has_ref_below =
    make_hasRefBelow ~transitive
      ~iter_value_refs_from:(References.iter_value_refs_from refs)
  in

  (* Process each declaration based on computed liveness *)
  let dead_declarations = ref [] in
  let inline_issues = ref [] in

  (* For consistent debug output, collect and sort declarations *)
  let all_decls =
    DeclarationStore.fold (fun _pos decl acc -> decl :: acc) decl_store []
    |> List.fast_sort Decl.compare_for_reporting
  in

  all_decls
  |> List.iter (fun (decl : Decl.t) ->
         let pos = decl.pos in
         let live_reason = Liveness.get_live_reason ~live pos in
         let is_live = Option.is_some live_reason in
         let is_dead = not is_live in

         (* Debug output (forward model):
            show reachability + why (root/propagated), and a compact dependency
            summary (incoming/outgoing declaration edges). *)
         if debug then (
           let status =
             match live_reason with
             | None -> "Dead"
             | Some reason ->
               Printf.sprintf "Live (%s)" (Liveness.reason_to_string reason)
           in
           Log_.item "%s %s %s@." status
             (decl.decl_kind |> Decl.Kind.to_string)
             (decl.path |> DcePath.to_string);
           (* Print dependency context to help understand why a decl is (not) live.
               This is declaration-to-declaration deps only, derived from refs_from. *)
           let outgoing_to_decls =
             match PosHash.find_opt decl_refs_index pos with
             | None -> 0
             | Some (value_targets, type_targets) ->
               let count_targets targets =
                 PosSet.fold
                   (fun target acc ->
                     match DeclarationStore.find_opt decl_store target with
                     | Some _ -> acc + 1
                     | None -> acc)
                   targets 0
               in
               count_targets value_targets + count_targets type_targets
           in
           let incoming_from_decls, incoming_from_live_decls =
             match PosHash.find_opt incoming_decl_deps pos with
             | None -> (0, 0)
             | Some sources ->
               let total = PosSet.cardinal sources in
               let live_src =
                 PosSet.fold
                   (fun src acc ->
                     if PosHash.mem live src then acc + 1 else acc)
                   sources 0
               in
               (total, live_src)
           in
           if incoming_from_decls > 0 || outgoing_to_decls > 0 then
             Log_.item "    deps: in=%d (live=%d dead=%d) out=%d@."
               incoming_from_decls incoming_from_live_decls
               (incoming_from_decls - incoming_from_live_decls)
               outgoing_to_decls;
           (* For debugging, print a small sample of incoming/outgoing decl deps.
               This is meant to answer: "what would make this decl live?" *)
           let max_show = 3 in
           (match PosHash.find_opt incoming_decl_deps pos with
           | None -> ()
           | Some sources ->
             let shown = ref 0 in
             PosSet.iter
               (fun src_pos ->
                 if !shown < max_show then (
                   incr shown;
                   match DeclarationStore.find_opt decl_store src_pos with
                   | Some src_decl ->
                     let src_status =
                       if PosHash.mem live src_pos then "live" else "dead"
                     in
                     Log_.item "      <- %s (%s)@."
                       (src_decl.path |> DcePath.to_string)
                       src_status
                   | None -> ()))
               sources;
             if PosSet.cardinal sources > max_show then
               Log_.item "      <- ... (%d more)@."
                 (PosSet.cardinal sources - max_show));
           match PosHash.find_opt decl_refs_index pos with
           | None -> ()
           | Some (value_targets, type_targets) ->
             let show_target target =
               match DeclarationStore.find_opt decl_store target with
               | None -> false
               | Some target_decl ->
                 Log_.item "      -> %s@." (target_decl.path |> DcePath.to_string);
                 true
             in
             let shown = ref 0 in
             let try_show targets =
               PosSet.iter
                 (fun target ->
                   if !shown < max_show then
                     if show_target target then incr shown)
                 targets
             in
             try_show value_targets;
             try_show type_targets;
             if outgoing_to_decls > max_show then
               Log_.item "      -> ... (%d more)@."
                 (outgoing_to_decls - max_show));

         decl.resolved_dead <- Some is_dead;

         if is_dead then (
           decl.path
           |> DeadModules.mark_dead ~config
                ~is_type:(decl.decl_kind |> Decl.Kind.is_type)
                ~loc:decl.module_loc;
           if not (do_report_dead ~ann_store decl.pos) then decl.report <- false;
           dead_declarations := decl :: !dead_declarations)
         else (
           (* Collect optional args issues for live declarations *)
           check_optional_arg_fn ~optional_args_state ~ann_store ~config decl
           |> List.iter (fun issue -> inline_issues := issue :: !inline_issues);
           decl.path
           |> DeadModules.mark_live ~config
                ~is_type:(decl.decl_kind |> Decl.Kind.is_type)
                ~loc:decl.module_loc;
           if AnnotationStore.is_annotated_dead ann_store decl.pos then (
             (* Collect incorrect @dead annotation issue *)
             let issue =
               make_dead_issue ~decl ~message:" is annotated @dead but is live"
                 IncorrectDeadAnnotation
             in
             decl.path
             |> DcePath.to_module_name ~is_type:(decl.decl_kind |> Decl.Kind.is_type)
             |> DeadModules.check_module_dead ~config ~file_name:decl.pos.pos_fname
             |> Option.iter (fun mod_issue ->
                    inline_issues := mod_issue :: !inline_issues);
             inline_issues := issue :: !inline_issues)));

  let sorted_dead_declarations =
    !dead_declarations |> List.fast_sort Decl.compare_for_reporting
  in

  (* Collect issues from dead declarations *)
  let reporting_ctx = ReportingContext.create () in
  let dead_issues =
    sorted_dead_declarations
    |> List.concat_map (fun decl ->
           report_declaration ~config ~has_ref_below reporting_ctx decl)
  in
  let all_issues = List.rev !inline_issues @ dead_issues in
  AnalysisResult.add_issues AnalysisResult.empty all_issues

(** Reactive solver using reactive liveness collection.
    [value_refs_from] is only needed when [transitive=false] for hasRefBelow.
    Pass [None] when [transitive=true] to avoid any refs computation. *)
let solve_dead_reactive ~ann_store ~config ~decl_store ~value_refs_from
    ~(live : (Lexing.position, unit) Reactive.t)
    ~(roots : (Lexing.position, unit) Reactive.t) ~optional_args_state
    ~check_optional_arg:
      (check_optional_arg_fn :
        optional_args_state:OptionalArgsState.t ->
        ann_store:AnnotationStore.t ->
        config:DceConfig.t ->
        Decl.t ->
        Issue.t list) : AnalysisResult.t =
  let t0 = Unix.gettimeofday () in
  let debug = config.DceConfig.cli.debug in
  let transitive = config.DceConfig.run.transitive in
  let is_live pos = Reactive.get live pos <> None in

  (* hasRefBelow uses on-demand search through value_refs_from *)
  let has_ref_below =
    match value_refs_from with
    | None -> fun _ -> false
    | Some refs_from ->
      make_hasRefBelow ~transitive ~iter_value_refs_from:(fun f ->
          Reactive.iter f refs_from)
  in

  (* Process each declaration based on computed liveness *)
  let dead_declarations = ref [] in
  let inline_issues = ref [] in

  let t1 = Unix.gettimeofday () in
  (* For consistent debug output, collect and sort declarations *)
  let all_decls =
    DeclarationStore.fold (fun _pos decl acc -> decl :: acc) decl_store []
  in
  let t2 = Unix.gettimeofday () in
  let all_decls = all_decls |> List.fast_sort Decl.compare_for_reporting in
  let t3 = Unix.gettimeofday () in
  let num_decls = List.length all_decls in

  (* Count operations in the loop *)
  let num_live_checks = ref 0 in
  let num_dead = ref 0 in
  let num_live = ref 0 in

  all_decls
  |> List.iter (fun (decl : Decl.t) ->
         let pos = decl.pos in
         incr num_live_checks;
         let is_live = is_live pos in
         let is_dead = not is_live in

         (* Debug output (forward model): derive root/propagated from [roots]. *)
         (if debug then
            let live_reason : Liveness.live_reason option =
              if not is_live then None
              else if Reactive.get roots pos <> None then
                if AnnotationStore.is_annotated_gentype_or_live ann_store pos
                then Some Liveness.Annotated
                else Some Liveness.ExternalRef
              else Some Liveness.Propagated
            in
            let status =
              match live_reason with
              | None -> "Dead"
              | Some reason ->
                Printf.sprintf "Live (%s)" (Liveness.reason_to_string reason)
            in
            Log_.item "%s %s %s@." status
              (decl.decl_kind |> Decl.Kind.to_string)
              (decl.path |> DcePath.to_string));

         decl.resolved_dead <- Some is_dead;

         if is_dead then (
           incr num_dead;
           decl.path
           |> DeadModules.mark_dead ~config
                ~is_type:(decl.decl_kind |> Decl.Kind.is_type)
                ~loc:decl.module_loc;
           if not (do_report_dead ~ann_store decl.pos) then decl.report <- false;
           dead_declarations := decl :: !dead_declarations)
         else (
           incr num_live;
           (* Collect optional args issues for live declarations *)
           check_optional_arg_fn ~optional_args_state ~ann_store ~config decl
           |> List.iter (fun issue -> inline_issues := issue :: !inline_issues);
           decl.path
           |> DeadModules.mark_live ~config
                ~is_type:(decl.decl_kind |> Decl.Kind.is_type)
                ~loc:decl.module_loc;
           if AnnotationStore.is_annotated_dead ann_store decl.pos then (
             (* Collect incorrect @dead annotation issue *)
             let issue =
               make_dead_issue ~decl ~message:" is annotated @dead but is live"
                 IncorrectDeadAnnotation
             in
             decl.path
             |> DcePath.to_module_name ~is_type:(decl.decl_kind |> Decl.Kind.is_type)
             |> DeadModules.check_module_dead ~config ~file_name:decl.pos.pos_fname
             |> Option.iter (fun mod_issue ->
                    inline_issues := mod_issue :: !inline_issues);
             inline_issues := issue :: !inline_issues)));
  let t4 = Unix.gettimeofday () in

  let sorted_dead_declarations =
    !dead_declarations |> List.fast_sort Decl.compare_for_reporting
  in
  let t5 = Unix.gettimeofday () in

  (* Collect issues from dead declarations *)
  let reporting_ctx = ReportingContext.create () in
  let dead_issues =
    sorted_dead_declarations
    |> List.concat_map (fun decl ->
           report_declaration ~config ~has_ref_below reporting_ctx decl)
  in
  let t6 = Unix.gettimeofday () in
  let all_issues = List.rev !inline_issues @ dead_issues in
  let t7 = Unix.gettimeofday () in

  Printf.eprintf
    "  solveDeadReactive timing breakdown:\n\
    \    setup:        %6.2fms\n\
    \    collect:      %6.2fms (DeclarationStore.fold)\n\
    \    sort:         %6.2fms (List.fast_sort %d decls)\n\
    \    iterate:      %6.2fms (check liveness for %d decls: %d dead, %d live)\n\
    \    sort_dead:    %6.2fms (sort %d dead decls)\n\
    \    report:       %6.2fms (generate issues)\n\
    \    combine:      %6.2fms\n\
    \    TOTAL:        %6.2fms\n"
    ((t1 -. t0) *. 1000.0)
    ((t2 -. t1) *. 1000.0)
    ((t3 -. t2) *. 1000.0)
    num_decls
    ((t4 -. t3) *. 1000.0)
    !num_live_checks !num_dead !num_live
    ((t5 -. t4) *. 1000.0)
    !num_dead
    ((t6 -. t5) *. 1000.0)
    ((t7 -. t6) *. 1000.0)
    ((t7 -. t0) *. 1000.0);

  AnalysisResult.add_issues AnalysisResult.empty all_issues

(** Main entry point - uses forward solver. *)
let solve_dead ~ann_store ~config ~decl_store ~ref_store ~optional_args_state
    ~check_optional_arg : AnalysisResult.t =
  match ReferenceStore.get_refs_opt ref_store with
  | Some refs ->
    solve_dead_forward ~ann_store ~config ~decl_store ~refs ~optional_args_state
      ~check_optional_arg
  | None ->
    failwith
      "solveDead: ReferenceStore must be Frozen (use solveDeadReactive for \
       reactive mode)"
