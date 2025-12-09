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
  let recursiveDebug = false
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

(* Helper functions for PosHash with PosSet values *)
let posHashFindSet h k = try PosHash.find h k with Not_found -> PosSet.empty

let posHashAddSet h k v =
  let set = posHashFindSet h k in
  PosHash.replace h k (PosSet.add v set)

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

(* NOTE: iterFilesFromRootsToLeaves moved to FileDeps.iter_files_from_roots_to_leaves *)

let iterFilesFromRootsToLeaves ~file_deps iterFun =
  FileDeps.iter_files_from_roots_to_leaves file_deps iterFun

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

(** Report a dead declaration. Returns list of issues (dead module first, then dead value).
      Caller is responsible for logging. *)
let reportDeclaration ~config ~refs (ctx : ReportingContext.t) decl :
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
    let hasRefBelow () =
      let decl_refs = References.find_value_refs refs decl.pos in
      let refIsBelow (pos : Lexing.position) =
        decl.pos.pos_fname <> pos.pos_fname
        || decl.pos.pos_cnum < pos.pos_cnum
           &&
           (* not a function defined inside a function, e.g. not a callback *)
           decl.posEnd.pos_cnum < pos.pos_cnum
      in
      decl_refs |> PosSet.exists refIsBelow
    in
    let shouldEmitWarning =
      (not insideReportedValue)
      && (match decl.path with
         | name :: _ when name |> Name.isUnderscore -> Config.reportUnderscore
         | _ -> true)
      && (config.DceConfig.run.transitive || not (hasRefBelow ()))
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

let declIsDead ~annotations ~refs decl =
  let liveRefs =
    refs
    |> PosSet.filter (fun p ->
           not (FileAnnotations.is_annotated_dead annotations p))
  in
  liveRefs |> PosSet.cardinal = 0
  && not
       (FileAnnotations.is_annotated_gentype_or_live annotations decl.Decl.pos)

let doReportDead ~annotations pos =
  not (FileAnnotations.is_annotated_gentype_or_dead annotations pos)

let rec resolveRecursiveRefs ~all_refs ~annotations ~config ~decls
    ~checkOptionalArg:
      (checkOptionalArgFn : config:DceConfig.t -> Decl.t -> Issue.t list)
    ~deadDeclarations ~issues ~level ~orderedFiles ~refs ~refsBeingResolved decl
    : bool =
  match decl.Decl.pos with
  | _ when decl.resolvedDead <> None ->
    if Config.recursiveDebug then
      Log_.item "recursiveDebug %s [%d] already resolved@."
        (decl.path |> DcePath.toString)
        level;
    (* Use the already-resolved value, not source annotations *)
    Option.get decl.resolvedDead
  | _ when PosSet.mem decl.pos !refsBeingResolved ->
    if Config.recursiveDebug then
      Log_.item "recursiveDebug %s [%d] is being resolved: assume dead@."
        (decl.path |> DcePath.toString)
        level;
    true
  | _ ->
    if Config.recursiveDebug then
      Log_.item "recursiveDebug resolving %s [%d]@."
        (decl.path |> DcePath.toString)
        level;
    refsBeingResolved := PosSet.add decl.pos !refsBeingResolved;
    let allDepsResolved = ref true in
    let newRefs =
      refs
      |> PosSet.filter (fun pos ->
             if pos = decl.pos then (
               if Config.recursiveDebug then
                 Log_.item "recursiveDebug %s ignoring reference to self@."
                   (decl.path |> DcePath.toString);
               false)
             else
               match Declarations.find_opt decls pos with
               | None ->
                 if Config.recursiveDebug then
                   Log_.item "recursiveDebug can't find decl for %s@."
                     (pos |> Pos.toString);
                 true
               | Some xDecl ->
                 let xRefs =
                   match xDecl.declKind |> Decl.Kind.isType with
                   | true -> References.find_type_refs all_refs pos
                   | false -> References.find_value_refs all_refs pos
                 in
                 let xDeclIsDead =
                   xDecl
                   |> resolveRecursiveRefs ~all_refs ~annotations ~config ~decls
                        ~checkOptionalArg:checkOptionalArgFn ~deadDeclarations
                        ~issues ~level:(level + 1) ~orderedFiles ~refs:xRefs
                        ~refsBeingResolved
                 in
                 if xDecl.resolvedDead = None then allDepsResolved := false;
                 not xDeclIsDead)
    in
    let isDead = decl |> declIsDead ~annotations ~refs:newRefs in
    let isResolved = (not isDead) || !allDepsResolved || level = 0 in
    if isResolved then (
      decl.resolvedDead <- Some isDead;
      if isDead then (
        decl.path
        |> DeadModules.markDead ~config
             ~isType:(decl.declKind |> Decl.Kind.isType)
             ~loc:decl.moduleLoc;
        if not (doReportDead ~annotations decl.pos) then decl.report <- false;
        deadDeclarations := decl :: !deadDeclarations)
      else (
        (* Collect optional args issues *)
        checkOptionalArgFn ~config decl
        |> List.iter (fun issue -> issues := issue :: !issues);
        decl.path
        |> DeadModules.markLive ~config
             ~isType:(decl.declKind |> Decl.Kind.isType)
             ~loc:decl.moduleLoc;
        if FileAnnotations.is_annotated_dead annotations decl.pos then (
          (* Collect incorrect @dead annotation issue *)
          let issue =
            makeDeadIssue ~decl ~message:" is annotated @dead but is live"
              IncorrectDeadAnnotation
          in
          decl.path
          |> DcePath.toModuleName ~isType:(decl.declKind |> Decl.Kind.isType)
          |> DeadModules.checkModuleDead ~config ~fileName:decl.pos.pos_fname
          |> Option.iter (fun mod_issue -> issues := mod_issue :: !issues);
          issues := issue :: !issues));
      if config.DceConfig.cli.debug then
        let refsString =
          newRefs |> PosSet.elements |> List.map Pos.toString
          |> String.concat ", "
        in
        Log_.item "%s %s %s: %d references (%s) [%d]@."
          (match isDead with
          | true -> "Dead"
          | false -> "Live")
          (decl.declKind |> Decl.Kind.toString)
          (decl.path |> DcePath.toString)
          (newRefs |> PosSet.cardinal)
          refsString level);
    isDead

let reportDead ~annotations ~config ~decls ~refs ~file_deps ~optional_args_state
    ~checkOptionalArg:
      (checkOptionalArgFn :
        optional_args_state:OptionalArgsState.t ->
        annotations:FileAnnotations.t ->
        config:DceConfig.t ->
        Decl.t ->
        Issue.t list) : AnalysisResult.t =
  let iterDeclInOrder ~deadDeclarations ~issues ~orderedFiles decl =
    let decl_refs =
      match decl |> Decl.isValue with
      | true -> References.find_value_refs refs decl.pos
      | false -> References.find_type_refs refs decl.pos
    in
    resolveRecursiveRefs ~all_refs:refs ~annotations ~config ~decls
      ~checkOptionalArg:(checkOptionalArgFn ~optional_args_state ~annotations)
      ~deadDeclarations ~issues ~level:0 ~orderedFiles
      ~refsBeingResolved:(ref PosSet.empty) ~refs:decl_refs decl
    |> ignore
  in
  if config.DceConfig.cli.debug then (
    Log_.item "@.File References@.@.";
    let fileList = ref [] in
    FileDeps.iter_deps file_deps (fun file files ->
        fileList := (file, files) :: !fileList);
    !fileList
    |> List.sort (fun (f1, _) (f2, _) -> String.compare f1 f2)
    |> List.iter (fun (file, files) ->
           Log_.item "%s -->> %s@."
             (file |> Filename.basename)
             (files |> FileSet.elements |> List.map Filename.basename
            |> String.concat ", ")));
  let declarations =
    Declarations.fold
      (fun _pos decl declarations -> decl :: declarations)
      decls []
  in
  let orderedFiles = Hashtbl.create 256 in
  iterFilesFromRootsToLeaves ~file_deps
    (let current = ref 0 in
     fun fileName ->
       incr current;
       Hashtbl.add orderedFiles fileName !current);
  let orderedDeclarations =
    (* analyze in reverse order *)
    declarations |> List.fast_sort (Decl.compareUsingDependencies ~orderedFiles)
  in
  let deadDeclarations = ref [] in
  let inline_issues = ref [] in
  orderedDeclarations
  |> List.iter
       (iterDeclInOrder ~orderedFiles ~deadDeclarations ~issues:inline_issues);
  let sortedDeadDeclarations =
    !deadDeclarations |> List.fast_sort Decl.compareForReporting
  in
  (* Collect issues from dead declarations *)
  let reporting_ctx = ReportingContext.create () in
  let dead_issues =
    sortedDeadDeclarations
    |> List.concat_map (fun decl ->
           reportDeclaration ~config ~refs reporting_ctx decl)
  in
  (* Combine all issues: inline issues first (they were logged during analysis),
     then dead declaration issues *)
  let all_issues = List.rev !inline_issues @ dead_issues in
  (* Return result - caller is responsible for logging *)
  AnalysisResult.add_issues AnalysisResult.empty all_issues
