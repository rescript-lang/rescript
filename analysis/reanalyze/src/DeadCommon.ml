module FileContext = struct
  type t = {source_path: string; module_name: string; is_interface: bool}

  (** Get module name as Name.t tagged with interface/implementation info *)
  let module_name_tagged file =
    file.module_name |> Name.create ~isInterface:file.is_interface
end

(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

module Config = struct
  (* Turn on type analysis *)
  let analyzeTypes = ref true
  let analyzeExternals = ref false
  let reportUnderscore = false
  let reportTypesDeadOnlyInInterface = false
  let warnOnCircularDependencies = false
end

let rec checkSub s1 s2 n =
  n <= 0
  || (try s1.[n] = s2.[n] with Invalid_argument _ -> false)
     && checkSub s1 s2 (n - 1)

let fileIsImplementationOf s1 s2 =
  let n1 = String.length s1 and n2 = String.length s2 in
  n2 = n1 + 1 && checkSub s1 s2 (n1 - 1)

let liveAnnotation = "live"

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

let declGetLoc decl =
  let loc_start =
    let offset =
      match decl.Decl.posAdjustment with
      | FirstVariant | Nothing -> 0
      | OtherVariant -> 2
    in
    let cnumWithOffset = decl.posStart.pos_cnum + offset in
    if cnumWithOffset < decl.posEnd.pos_cnum then
      {decl.posStart with pos_cnum = cnumWithOffset}
    else decl.posStart
  in
  {Location.loc_start; loc_end = decl.posEnd; loc_ghost = false}

let addValueReference ~config ~refs ~file_deps ~(binding : Location.t)
    ~addFileReference ~(locFrom : Location.t) ~(locTo : Location.t) : unit =
  let effectiveFrom = if binding = Location.none then locFrom else binding in
  if not effectiveFrom.loc_ghost then (
    if config.DceConfig.cli.debug then
      Log_.item "addValueReference %s --> %s@."
        (effectiveFrom.loc_start |> Pos.toString)
        (locTo.loc_start |> Pos.toString);
    References.add_value_ref refs ~posTo:locTo.loc_start
      ~posFrom:effectiveFrom.loc_start;
    if
      addFileReference && (not locTo.loc_ghost)
      && (not effectiveFrom.loc_ghost)
      && effectiveFrom.loc_start.pos_fname <> locTo.loc_start.pos_fname
    then
      FileDeps.add_dep file_deps ~from_file:effectiveFrom.loc_start.pos_fname
        ~to_file:locTo.loc_start.pos_fname)

let addDeclaration_ ~config ~decls ~(file : FileContext.t) ?posEnd ?posStart
    ~declKind ~path ~(loc : Location.t) ?(posAdjustment = Decl.Nothing)
    ~moduleLoc (name : Name.t) =
  let pos = loc.loc_start in
  let posStart =
    match posStart with
    | Some posStart -> posStart
    | None -> pos
  in
  let posEnd =
    match posEnd with
    | Some posEnd -> posEnd
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
        (declKind |> Decl.Kind.toString)
        (name |> Name.toString) (pos |> Pos.toString) (path |> DcePath.toString);
    let decl =
      {
        Decl.declKind;
        moduleLoc;
        posAdjustment;
        path = name :: path;
        pos;
        posEnd;
        posStart;
        resolvedDead = None;
        report = true;
      }
    in
    Declarations.add decls pos decl)

let addValueDeclaration ~config ~decls ~file ?(isToplevel = true)
    ~(loc : Location.t) ~moduleLoc ?(optionalArgs = OptionalArgs.empty) ~path
    ~sideEffects name =
  name
  |> addDeclaration_ ~config ~decls ~file
       ~declKind:(Value {isToplevel; optionalArgs; sideEffects})
       ~loc ~moduleLoc ~path

(** Create a dead code issue. Pure - no side effects. *)
let makeDeadIssue ~decl ~message deadWarning : Issue.t =
  let loc = decl |> declGetLoc in
  AnalysisResult.make_dead_issue ~loc ~deadWarning
    ~path:(DcePath.withoutHead decl.path)
    ~message

let isInsideReportedValue (ctx : ReportingContext.t) decl =
  let max_end = ReportingContext.get_max_end ctx in
  let fileHasChanged = max_end.pos_fname <> decl.Decl.pos.pos_fname in
  let insideReportedValue =
    decl |> Decl.isValue && (not fileHasChanged)
    && max_end.pos_cnum > decl.pos.pos_cnum
  in
  if not insideReportedValue then
    if decl |> Decl.isValue then
      if fileHasChanged || decl.posEnd.pos_cnum > max_end.pos_cnum then
        ReportingContext.set_max_end ctx decl.posEnd;
  insideReportedValue

(** Check if a reference position is "below" the declaration.
    A ref is below if it's in a different file, or comes after the declaration
    (but not inside it, e.g. not a callback). *)
let refIsBelow (decl : Decl.t) (posFrom : Lexing.position) =
  decl.pos.pos_fname <> posFrom.pos_fname
  || decl.pos.pos_cnum < posFrom.pos_cnum
     &&
     (* not a function defined inside a function, e.g. not a callback *)
     decl.posEnd.pos_cnum < posFrom.pos_cnum

(** Create hasRefBelow function using on-demand per-decl search.
    [iter_value_refs_from] iterates over (posFrom, posToSet) pairs.
    O(total_refs) per dead decl, but dead decls should be few. *)
let make_hasRefBelow ~transitive ~iter_value_refs_from =
  if transitive then fun _ -> false
  else fun decl ->
    let found = ref false in
    iter_value_refs_from (fun posFrom posToSet ->
        if (not !found) && PosSet.mem decl.Decl.pos posToSet then
          if refIsBelow decl posFrom then found := true);
    !found

(** Report a dead declaration. Returns list of issues (dead module first, then dead value).
    [hasRefBelow] checks if there are references from "below" the declaration.
    Only used when [config.run.transitive] is false. *)
let reportDeclaration ~config ~hasRefBelow (ctx : ReportingContext.t) decl :
    Issue.t list =
  let insideReportedValue = decl |> isInsideReportedValue ctx in
  if not decl.report then []
  else
    let deadWarning, message =
      match decl.declKind with
      | Exception ->
        (Issue.WarningDeadException, "is never raised or passed as value")
      | Value {sideEffects} -> (
        let noSideEffectsOrUnderscore =
          (not sideEffects)
          ||
          match decl.path with
          | hd :: _ -> hd |> Name.startsWithUnderscore
          | [] -> false
        in
        ( (match not noSideEffectsOrUnderscore with
          | true -> WarningDeadValueWithSideEffects
          | false -> WarningDeadValue),
          match decl.path with
          | name :: _ when name |> Name.isUnderscore ->
            "has no side effects and can be removed"
          | _ -> (
            "is never used"
            ^
            match not noSideEffectsOrUnderscore with
            | true -> " and could have side effects"
            | false -> "") ))
      | RecordLabel ->
        (WarningDeadType, "is a record label never used to read a value")
      | VariantCase ->
        (WarningDeadType, "is a variant case which is never constructed")
    in
    let shouldEmitWarning =
      (not insideReportedValue)
      && (match decl.path with
         | name :: _ when name |> Name.isUnderscore -> Config.reportUnderscore
         | _ -> true)
      && (config.DceConfig.run.transitive || not (hasRefBelow decl))
    in
    if shouldEmitWarning then
      let dead_module_issue =
        decl.path
        |> DcePath.toModuleName ~isType:(decl.declKind |> Decl.Kind.isType)
        |> DeadModules.checkModuleDead ~config ~fileName:decl.pos.pos_fname
      in
      let dead_value_issue = makeDeadIssue ~decl ~message deadWarning in
      (* Return in order: dead module first (if any), then dead value *)
      match dead_module_issue with
      | Some mi -> [mi; dead_value_issue]
      | None -> [dead_value_issue]
    else []

let doReportDead ~ann_store pos =
  not (AnnotationStore.is_annotated_gentype_or_dead ann_store pos)

(** Forward-based solver using refs_from direction.
    Computes liveness via forward propagation, then processes declarations. *)
let solveDeadForward ~ann_store ~config ~decl_store ~refs ~optional_args_state
    ~checkOptionalArg:
      (checkOptionalArgFn :
        optional_args_state:OptionalArgsState.t ->
        ann_store:AnnotationStore.t ->
        config:DceConfig.t ->
        Decl.t ->
        Issue.t list) : AnalysisResult.t =
  (* Compute liveness using forward propagation *)
  let debug = config.DceConfig.cli.debug in
  let transitive = config.DceConfig.run.transitive in
  let live = Liveness.compute_forward ~debug ~decl_store ~refs ~ann_store in

  (* hasRefBelow uses on-demand search through refs_from *)
  let hasRefBelow =
    make_hasRefBelow ~transitive
      ~iter_value_refs_from:(References.iter_value_refs_from refs)
  in

  (* Process each declaration based on computed liveness *)
  let deadDeclarations = ref [] in
  let inline_issues = ref [] in

  (* For consistent debug output, collect and sort declarations *)
  let all_decls =
    DeclarationStore.fold (fun _pos decl acc -> decl :: acc) decl_store []
    |> List.fast_sort Decl.compareForReporting
  in

  all_decls
  |> List.iter (fun (decl : Decl.t) ->
         let pos = decl.pos in
         let live_reason = Liveness.get_live_reason ~live pos in
         let is_live = Option.is_some live_reason in
         let is_dead = not is_live in

         (* Debug output (forward model):
            show reachability + why (root/propagated), without inverse refs. *)
         (if debug then
            let status =
              match live_reason with
              | None -> "Dead"
              | Some reason ->
                Printf.sprintf "Live (%s)" (Liveness.reason_to_string reason)
            in
            Log_.item "%s %s %s@." status
              (decl.declKind |> Decl.Kind.toString)
              (decl.path |> DcePath.toString));

         decl.resolvedDead <- Some is_dead;

         if is_dead then (
           decl.path
           |> DeadModules.markDead ~config
                ~isType:(decl.declKind |> Decl.Kind.isType)
                ~loc:decl.moduleLoc;
           if not (doReportDead ~ann_store decl.pos) then decl.report <- false;
           deadDeclarations := decl :: !deadDeclarations)
         else (
           (* Collect optional args issues for live declarations *)
           checkOptionalArgFn ~optional_args_state ~ann_store ~config decl
           |> List.iter (fun issue -> inline_issues := issue :: !inline_issues);
           decl.path
           |> DeadModules.markLive ~config
                ~isType:(decl.declKind |> Decl.Kind.isType)
                ~loc:decl.moduleLoc;
           if AnnotationStore.is_annotated_dead ann_store decl.pos then (
             (* Collect incorrect @dead annotation issue *)
             let issue =
               makeDeadIssue ~decl ~message:" is annotated @dead but is live"
                 IncorrectDeadAnnotation
             in
             decl.path
             |> DcePath.toModuleName ~isType:(decl.declKind |> Decl.Kind.isType)
             |> DeadModules.checkModuleDead ~config ~fileName:decl.pos.pos_fname
             |> Option.iter (fun mod_issue ->
                    inline_issues := mod_issue :: !inline_issues);
             inline_issues := issue :: !inline_issues)));

  let sortedDeadDeclarations =
    !deadDeclarations |> List.fast_sort Decl.compareForReporting
  in

  (* Collect issues from dead declarations *)
  let reporting_ctx = ReportingContext.create () in
  let dead_issues =
    sortedDeadDeclarations
    |> List.concat_map (fun decl ->
           reportDeclaration ~config ~hasRefBelow reporting_ctx decl)
  in
  let all_issues = List.rev !inline_issues @ dead_issues in
  AnalysisResult.add_issues AnalysisResult.empty all_issues

(** Reactive solver using reactive liveness collection.
    [value_refs_from] is only needed when [transitive=false] for hasRefBelow.
    Pass [None] when [transitive=true] to avoid any refs computation. *)
let solveDeadReactive ~ann_store ~config ~decl_store ~value_refs_from
    ~(live : (Lexing.position, unit) Reactive.t)
    ~(roots : (Lexing.position, unit) Reactive.t) ~optional_args_state
    ~checkOptionalArg:
      (checkOptionalArgFn :
        optional_args_state:OptionalArgsState.t ->
        ann_store:AnnotationStore.t ->
        config:DceConfig.t ->
        Decl.t ->
        Issue.t list) : AnalysisResult.t =
  let debug = config.DceConfig.cli.debug in
  let transitive = config.DceConfig.run.transitive in
  let is_live pos = Reactive.get live pos <> None in

  (* hasRefBelow uses on-demand search through value_refs_from *)
  let hasRefBelow =
    match value_refs_from with
    | None -> fun _ -> false
    | Some refs_from ->
      make_hasRefBelow ~transitive ~iter_value_refs_from:(fun f ->
          Reactive.iter f refs_from)
  in

  (* Process each declaration based on computed liveness *)
  let deadDeclarations = ref [] in
  let inline_issues = ref [] in

  (* For consistent debug output, collect and sort declarations *)
  let all_decls =
    DeclarationStore.fold (fun _pos decl acc -> decl :: acc) decl_store []
    |> List.fast_sort Decl.compareForReporting
  in

  all_decls
  |> List.iter (fun (decl : Decl.t) ->
         let pos = decl.pos in
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
              (decl.declKind |> Decl.Kind.toString)
              (decl.path |> DcePath.toString));

         decl.resolvedDead <- Some is_dead;

         if is_dead then (
           decl.path
           |> DeadModules.markDead ~config
                ~isType:(decl.declKind |> Decl.Kind.isType)
                ~loc:decl.moduleLoc;
           if not (doReportDead ~ann_store decl.pos) then decl.report <- false;
           deadDeclarations := decl :: !deadDeclarations)
         else (
           (* Collect optional args issues for live declarations *)
           checkOptionalArgFn ~optional_args_state ~ann_store ~config decl
           |> List.iter (fun issue -> inline_issues := issue :: !inline_issues);
           decl.path
           |> DeadModules.markLive ~config
                ~isType:(decl.declKind |> Decl.Kind.isType)
                ~loc:decl.moduleLoc;
           if AnnotationStore.is_annotated_dead ann_store decl.pos then (
             (* Collect incorrect @dead annotation issue *)
             let issue =
               makeDeadIssue ~decl ~message:" is annotated @dead but is live"
                 IncorrectDeadAnnotation
             in
             decl.path
             |> DcePath.toModuleName ~isType:(decl.declKind |> Decl.Kind.isType)
             |> DeadModules.checkModuleDead ~config ~fileName:decl.pos.pos_fname
             |> Option.iter (fun mod_issue ->
                    inline_issues := mod_issue :: !inline_issues);
             inline_issues := issue :: !inline_issues)));

  let sortedDeadDeclarations =
    !deadDeclarations |> List.fast_sort Decl.compareForReporting
  in

  (* Collect issues from dead declarations *)
  let reporting_ctx = ReportingContext.create () in
  let dead_issues =
    sortedDeadDeclarations
    |> List.concat_map (fun decl ->
           reportDeclaration ~config ~hasRefBelow reporting_ctx decl)
  in
  let all_issues = List.rev !inline_issues @ dead_issues in
  AnalysisResult.add_issues AnalysisResult.empty all_issues

(** Main entry point - uses forward solver. *)
let solveDead ~ann_store ~config ~decl_store ~ref_store ~optional_args_state
    ~checkOptionalArg : AnalysisResult.t =
  match ReferenceStore.get_refs_opt ref_store with
  | Some refs ->
    solveDeadForward ~ann_store ~config ~decl_store ~refs ~optional_args_state
      ~checkOptionalArg
  | None ->
    failwith
      "solveDead: ReferenceStore must be Frozen (use solveDeadReactive for \
       reactive mode)"
