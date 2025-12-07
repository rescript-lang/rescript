module FileContext = struct
  type t = {source_path: string; module_name: string; is_interface: bool}

  (** Get module name as Name.t tagged with interface/implementation info *)
  let module_name_tagged file =
    file.module_name |> Name.create ~isInterface:file.is_interface
end

(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open Common

module PosSet = Set.Make (struct
  type t = Lexing.position

  let compare = compare
end)

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

module PosHash = struct
  include Hashtbl.Make (struct
    type t = Lexing.position

    let hash x =
      let s = Filename.basename x.Lexing.pos_fname in
      Hashtbl.hash (x.Lexing.pos_cnum, s)

    let equal (x : t) y = x = y
  end)

  let findSet h k = try find h k with Not_found -> PosSet.empty

  let addSet h k v =
    let set = findSet h k in
    replace h k (PosSet.add v set)
end

type decls = decl PosHash.t
(** all exported declarations *)

let decls = (PosHash.create 256 : decls)

module ValueReferences = struct
  (** all value references *)
  let table = (PosHash.create 256 : PosSet.t PosHash.t)

  let add posTo posFrom = PosHash.addSet table posTo posFrom
  let find pos = PosHash.findSet table pos
end

(* Local reporting context used only while emitting dead-code warnings.
   It tracks, per file, the end position of the last value we reported on,
   so nested values inside that range don't get duplicate warnings. *)
module ReportingContext = struct
  type t = Lexing.position ref

  let create () : t = ref Lexing.dummy_pos
  let get_max_end (ctx : t) = !ctx
  let set_max_end (ctx : t) (pos : Lexing.position) = ctx := pos
end

module TypeReferences = struct
  (** all type references *)
  let table = (PosHash.create 256 : PosSet.t PosHash.t)

  let add posTo posFrom = PosHash.addSet table posTo posFrom
  let find pos = PosHash.findSet table pos
end

let declGetLoc decl =
  let loc_start =
    let offset =
      WriteDeadAnnotations.offsetOfPosAdjustment decl.posAdjustment
    in
    let cnumWithOffset = decl.posStart.pos_cnum + offset in
    if cnumWithOffset < decl.posEnd.pos_cnum then
      {decl.posStart with pos_cnum = cnumWithOffset}
    else decl.posStart
  in
  {Location.loc_start; loc_end = decl.posEnd; loc_ghost = false}

let addValueReference ~config ~(binding : Location.t) ~addFileReference
    ~(locFrom : Location.t) ~(locTo : Location.t) : unit =
  let effectiveFrom = if binding = Location.none then locFrom else binding in
  if not effectiveFrom.loc_ghost then (
    if config.DceConfig.cli.debug then
      Log_.item "addValueReference %s --> %s@."
        (effectiveFrom.loc_start |> posToString)
        (locTo.loc_start |> posToString);
    ValueReferences.add locTo.loc_start effectiveFrom.loc_start;
    if
      addFileReference && (not locTo.loc_ghost)
      && (not effectiveFrom.loc_ghost)
      && effectiveFrom.loc_start.pos_fname <> locTo.loc_start.pos_fname
    then FileReferences.add effectiveFrom locTo)

let iterFilesFromRootsToLeaves iterFun =
  (* For each file, the number of incoming references *)
  let inverseReferences = (Hashtbl.create 1 : (string, int) Hashtbl.t) in
  (* For each number of incoming references, the files *)
  let referencesByNumber = (Hashtbl.create 1 : (int, FileSet.t) Hashtbl.t) in
  let getNum fileName =
    try Hashtbl.find inverseReferences fileName with Not_found -> 0
  in
  let getSet num =
    try Hashtbl.find referencesByNumber num with Not_found -> FileSet.empty
  in
  let addIncomingEdge fileName =
    let oldNum = getNum fileName in
    let newNum = oldNum + 1 in
    let oldSetAtNum = getSet oldNum in
    let newSetAtNum = FileSet.remove fileName oldSetAtNum in
    let oldSetAtNewNum = getSet newNum in
    let newSetAtNewNum = FileSet.add fileName oldSetAtNewNum in
    Hashtbl.replace inverseReferences fileName newNum;
    Hashtbl.replace referencesByNumber oldNum newSetAtNum;
    Hashtbl.replace referencesByNumber newNum newSetAtNewNum
  in
  let removeIncomingEdge fileName =
    let oldNum = getNum fileName in
    let newNum = oldNum - 1 in
    let oldSetAtNum = getSet oldNum in
    let newSetAtNum = FileSet.remove fileName oldSetAtNum in
    let oldSetAtNewNum = getSet newNum in
    let newSetAtNewNum = FileSet.add fileName oldSetAtNewNum in
    Hashtbl.replace inverseReferences fileName newNum;
    Hashtbl.replace referencesByNumber oldNum newSetAtNum;
    Hashtbl.replace referencesByNumber newNum newSetAtNewNum
  in
  let addEdge fromFile toFile =
    if FileReferences.exists fromFile then addIncomingEdge toFile
  in
  let removeEdge fromFile toFile =
    if FileReferences.exists fromFile then removeIncomingEdge toFile
  in
  FileReferences.iter (fun fromFile set ->
      if getNum fromFile = 0 then
        Hashtbl.replace referencesByNumber 0 (FileSet.add fromFile (getSet 0));
      set |> FileSet.iter (fun toFile -> addEdge fromFile toFile));
  while getSet 0 <> FileSet.empty do
    let filesWithNoIncomingReferences = getSet 0 in
    Hashtbl.remove referencesByNumber 0;
    filesWithNoIncomingReferences
    |> FileSet.iter (fun fileName ->
           iterFun fileName;
           let references = FileReferences.find fileName in
           references |> FileSet.iter (fun toFile -> removeEdge fileName toFile))
  done;
  (* Process any remaining items in case of circular references *)
  referencesByNumber
  |> Hashtbl.iter (fun _num set ->
         if FileSet.is_empty set then ()
         else
           set
           |> FileSet.iter (fun fileName ->
                  let pos = {Lexing.dummy_pos with pos_fname = fileName} in
                  let loc =
                    {Location.none with loc_start = pos; loc_end = pos}
                  in
                  if Config.warnOnCircularDependencies then
                    Log_.warning ~loc
                      (Circular
                         {
                           message =
                             Format.asprintf
                               "Results for %s could be inaccurate because of \
                                circular references"
                               fileName;
                         });
                  iterFun fileName))

let addDeclaration_ ~config ~(file : FileContext.t) ?posEnd ?posStart ~declKind
    ~path ~(loc : Location.t) ?(posAdjustment = Nothing) ~moduleLoc
    (name : Name.t) =
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
        (declKind |> DeclKind.toString)
        (name |> Name.toString) (pos |> posToString) (path |> Path.toString);
    let decl =
      {
        declKind;
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
    PosHash.replace decls pos decl)

let addValueDeclaration ~config ~file ?(isToplevel = true) ~(loc : Location.t)
    ~moduleLoc ?(optionalArgs = OptionalArgs.empty) ~path ~sideEffects name =
  name
  |> addDeclaration_ ~config ~file
       ~declKind:(Value {isToplevel; optionalArgs; sideEffects})
       ~loc ~moduleLoc ~path

let emitWarning ~config ~decl ~message deadWarning =
  let loc = decl |> declGetLoc in
  let isToplevelValueWithSideEffects decl =
    match decl.declKind with
    | Value {isToplevel; sideEffects} -> isToplevel && sideEffects
    | _ -> false
  in
  let shouldWriteLineAnnotation =
    (not (isToplevelValueWithSideEffects decl))
    && Suppress.filter decl.pos
    && deadWarning <> IncorrectDeadAnnotation
  in
  let lineAnnotation =
    if shouldWriteLineAnnotation then
      WriteDeadAnnotations.addLineAnnotation ~config ~decl
    else None
  in
  decl.path
  |> Path.toModuleName ~isType:(decl.declKind |> DeclKind.isType)
  |> DeadModules.checkModuleDead ~config ~fileName:decl.pos.pos_fname;
  Log_.warning ~loc
    (DeadWarning
       {
         deadWarning;
         path = Path.withoutHead decl.path;
         message;
         lineAnnotation;
         shouldWriteLineAnnotation;
       })

module Decl = struct
  let isValue decl =
    match decl.declKind with
    | Value _ (* | Exception *) -> true
    | _ -> false

  let isToplevelValueWithSideEffects decl =
    match decl.declKind with
    | Value {isToplevel; sideEffects} -> isToplevel && sideEffects
    | _ -> false

  let compareUsingDependencies ~orderedFiles
      {
        declKind = kind1;
        path = _path1;
        pos =
          {
            pos_fname = fname1;
            pos_lnum = lnum1;
            pos_bol = bol1;
            pos_cnum = cnum1;
          };
      }
      {
        declKind = kind2;
        path = _path2;
        pos =
          {
            pos_fname = fname2;
            pos_lnum = lnum2;
            pos_bol = bol2;
            pos_cnum = cnum2;
          };
      } =
    let findPosition fn = Hashtbl.find orderedFiles fn [@@raises Not_found] in
    (* From the root of the file dependency DAG to the leaves.
       From the bottom of the file to the top. *)
    let position1, position2 =
      try (fname1 |> findPosition, fname2 |> findPosition)
      with Not_found -> (0, 0)
    in
    compare
      (position1, lnum2, bol2, cnum2, kind1)
      (position2, lnum1, bol1, cnum1, kind2)

  let compareForReporting
      {
        declKind = kind1;
        pos =
          {
            pos_fname = fname1;
            pos_lnum = lnum1;
            pos_bol = bol1;
            pos_cnum = cnum1;
          };
      }
      {
        declKind = kind2;
        pos =
          {
            pos_fname = fname2;
            pos_lnum = lnum2;
            pos_bol = bol2;
            pos_cnum = cnum2;
          };
      } =
    compare
      (fname1, lnum1, bol1, cnum1, kind1)
      (fname2, lnum2, bol2, cnum2, kind2)

  let isInsideReportedValue (ctx : ReportingContext.t) decl =
    let max_end = ReportingContext.get_max_end ctx in
    let fileHasChanged = max_end.pos_fname <> decl.pos.pos_fname in
    let insideReportedValue =
      decl |> isValue && (not fileHasChanged)
      && max_end.pos_cnum > decl.pos.pos_cnum
    in
    if not insideReportedValue then
      if decl |> isValue then
        if fileHasChanged || decl.posEnd.pos_cnum > max_end.pos_cnum then
          ReportingContext.set_max_end ctx decl.posEnd;
    insideReportedValue

  let report ~config (ctx : ReportingContext.t) decl =
    let insideReportedValue = decl |> isInsideReportedValue ctx in
    if decl.report then
      let name, message =
        match decl.declKind with
        | Exception ->
          (WarningDeadException, "is never raised or passed as value")
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
        let refs = ValueReferences.find decl.pos in
        let refIsBelow (pos : Lexing.position) =
          decl.pos.pos_fname <> pos.pos_fname
          || decl.pos.pos_cnum < pos.pos_cnum
             &&
             (* not a function defined inside a function, e.g. not a callback *)
             decl.posEnd.pos_cnum < pos.pos_cnum
        in
        refs |> PosSet.exists refIsBelow
      in
      let shouldEmitWarning =
        (not insideReportedValue)
        && (match decl.path with
           | name :: _ when name |> Name.isUnderscore -> Config.reportUnderscore
           | _ -> true)
        && (config.DceConfig.run.transitive || not (hasRefBelow ()))
      in
      if shouldEmitWarning then (
        decl.path
        |> Path.toModuleName ~isType:(decl.declKind |> DeclKind.isType)
        |> DeadModules.checkModuleDead ~config ~fileName:decl.pos.pos_fname;
        emitWarning ~config ~decl ~message name)
end

let declIsDead ~annotations ~refs decl =
  let liveRefs =
    refs
    |> PosSet.filter (fun p ->
           not (FileAnnotations.is_annotated_dead annotations p))
  in
  liveRefs |> PosSet.cardinal = 0
  && not (FileAnnotations.is_annotated_gentype_or_live annotations decl.pos)

let doReportDead ~annotations pos =
  not (FileAnnotations.is_annotated_gentype_or_dead annotations pos)

let rec resolveRecursiveRefs ~annotations ~config
    ~checkOptionalArg:(checkOptionalArgFn : config:DceConfig.t -> decl -> unit)
    ~deadDeclarations ~level ~orderedFiles ~refs ~refsBeingResolved decl : bool
    =
  match decl.pos with
  | _ when decl.resolvedDead <> None ->
    if Config.recursiveDebug then
      Log_.item "recursiveDebug %s [%d] already resolved@."
        (decl.path |> Path.toString)
        level;
    (* Use the already-resolved value, not source annotations *)
    Option.get decl.resolvedDead
  | _ when PosSet.mem decl.pos !refsBeingResolved ->
    if Config.recursiveDebug then
      Log_.item "recursiveDebug %s [%d] is being resolved: assume dead@."
        (decl.path |> Path.toString)
        level;
    true
  | _ ->
    if Config.recursiveDebug then
      Log_.item "recursiveDebug resolving %s [%d]@."
        (decl.path |> Path.toString)
        level;
    refsBeingResolved := PosSet.add decl.pos !refsBeingResolved;
    let allDepsResolved = ref true in
    let newRefs =
      refs
      |> PosSet.filter (fun pos ->
             if pos = decl.pos then (
               if Config.recursiveDebug then
                 Log_.item "recursiveDebug %s ignoring reference to self@."
                   (decl.path |> Path.toString);
               false)
             else
               match PosHash.find_opt decls pos with
               | None ->
                 if Config.recursiveDebug then
                   Log_.item "recursiveDebug can't find decl for %s@."
                     (pos |> posToString);
                 true
               | Some xDecl ->
                 let xRefs =
                   match xDecl.declKind |> DeclKind.isType with
                   | true -> TypeReferences.find pos
                   | false -> ValueReferences.find pos
                 in
                 let xDeclIsDead =
                   xDecl
                   |> resolveRecursiveRefs ~annotations ~config
                        ~checkOptionalArg:checkOptionalArgFn ~deadDeclarations
                        ~level:(level + 1) ~orderedFiles ~refs:xRefs
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
             ~isType:(decl.declKind |> DeclKind.isType)
             ~loc:decl.moduleLoc;
        if not (doReportDead ~annotations decl.pos) then decl.report <- false;
        deadDeclarations := decl :: !deadDeclarations)
      else (
        checkOptionalArgFn ~config decl;
        decl.path
        |> DeadModules.markLive ~config
             ~isType:(decl.declKind |> DeclKind.isType)
             ~loc:decl.moduleLoc;
        if FileAnnotations.is_annotated_dead annotations decl.pos then
          emitWarning ~config ~decl ~message:" is annotated @dead but is live"
            IncorrectDeadAnnotation);
      if config.DceConfig.cli.debug then
        let refsString =
          newRefs |> PosSet.elements |> List.map posToString
          |> String.concat ", "
        in
        Log_.item "%s %s %s: %d references (%s) [%d]@."
          (match isDead with
          | true -> "Dead"
          | false -> "Live")
          (decl.declKind |> DeclKind.toString)
          (decl.path |> Path.toString)
          (newRefs |> PosSet.cardinal)
          refsString level);
    isDead

let reportDead ~annotations ~config
    ~checkOptionalArg:
      (checkOptionalArgFn :
        annotations:FileAnnotations.t -> config:DceConfig.t -> decl -> unit) =
  let iterDeclInOrder ~deadDeclarations ~orderedFiles decl =
    let refs =
      match decl |> Decl.isValue with
      | true -> ValueReferences.find decl.pos
      | false -> TypeReferences.find decl.pos
    in
    resolveRecursiveRefs ~annotations ~config
      ~checkOptionalArg:(checkOptionalArgFn ~annotations)
      ~deadDeclarations ~level:0 ~orderedFiles
      ~refsBeingResolved:(ref PosSet.empty) ~refs decl
    |> ignore
  in
  if config.DceConfig.cli.debug then (
    Log_.item "@.File References@.@.";
    let fileList = ref [] in
    FileReferences.iter (fun file files ->
        fileList := (file, files) :: !fileList);
    !fileList
    |> List.sort (fun (f1, _) (f2, _) -> String.compare f1 f2)
    |> List.iter (fun (file, files) ->
           Log_.item "%s -->> %s@."
             (file |> Filename.basename)
             (files |> FileSet.elements |> List.map Filename.basename
            |> String.concat ", ")));
  let declarations =
    PosHash.fold (fun _pos decl declarations -> decl :: declarations) decls []
  in
  let orderedFiles = Hashtbl.create 256 in
  iterFilesFromRootsToLeaves
    (let current = ref 0 in
     fun fileName ->
       incr current;
       Hashtbl.add orderedFiles fileName !current);
  let orderedDeclarations =
    (* analyze in reverse order *)
    declarations |> List.fast_sort (Decl.compareUsingDependencies ~orderedFiles)
  in
  let deadDeclarations = ref [] in
  orderedDeclarations
  |> List.iter (iterDeclInOrder ~orderedFiles ~deadDeclarations);
  let sortedDeadDeclarations =
    !deadDeclarations |> List.fast_sort Decl.compareForReporting
  in
  let reporting_ctx = ReportingContext.create () in
  sortedDeadDeclarations |> List.iter (Decl.report ~config reporting_ctx)
