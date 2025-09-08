open SharedTypes

let debugReferences = ref true
let maybeLog m = if !debugReferences then Log.log ("[ref] " ^ m)

let checkPos (line, char)
    {Location.loc_start = {pos_lnum; pos_bol; pos_cnum}; loc_end} =
  if line < pos_lnum || (line = pos_lnum && char < pos_cnum - pos_bol) then
    false
  else if
    line > loc_end.pos_lnum
    || (line = loc_end.pos_lnum && char > loc_end.pos_cnum - loc_end.pos_bol)
  then false
  else true

let locItemsForPos ~extra pos =
  extra.locItems |> List.filter (fun {loc; locType = _} -> checkPos pos loc)

let lineColToCmtLoc ~pos:(line, col) = (line + 1, col)

let getLocItem ~full ~pos ~debug =
  let log n msg = if debug then Printf.printf "getLocItem #%d: %s\n" n msg in
  let pos = lineColToCmtLoc ~pos in
  let locItems = locItemsForPos ~extra:full.extra pos in
  if !Log.verbose then
    print_endline
      ("locItems:\n  "
      ^ (locItems |> List.map locItemToString |> String.concat "\n  "));
  let nameOf li =
    match li.locType with
    | Typed (n, _, _) -> n
    | _ -> "NotFound"
  in
  match locItems with
  | li1 :: li2 :: li3 :: ({locType = Typed ("makeProps", _, _)} as li4) :: _
    when full.file.uri |> Uri.isInterface ->
    log 1 "heuristic for makeProps in interface files";
    if debug then
      Printf.printf "n1:%s n2:%s n3:%s\n" (nameOf li1) (nameOf li2) (nameOf li3);
    Some li4
  | [
   {locType = Constant _};
   ({locType = Typed ("createDOMElementVariadic", _, _)} as li2);
  ] ->
    log 3 "heuristic for <div>";
    Some li2
  | {locType = Typed ("makeProps", _, _)}
    :: ({locType = Typed ("make", _, _)} as li2)
    :: _ ->
    log 4
      "heuristic for </Comp> within fragments: take make as makeProps does not \
       work\n\
       the type is not great but jump to definition works";
    Some li2
  | [
   ({locType = Typed (_, _, LocalReference _)} as li1);
   ({locType = Typed (_, _, _)} as li2);
  ]
    when li1.loc = li2.loc ->
    log 5
      "heuristic for JSX and compiler combined:\n\
       ~x becomes Props#x\n\
       heuristic for: [Props, x], give loc of `x`";
    if debug then Printf.printf "n1:%s n2:%s\n" (nameOf li1) (nameOf li2);
    Some li2
  | [
   ({locType = Typed (_, _, LocalReference _)} as li1);
   ({locType = Typed (_, _, GlobalReference ("Js_OO", ["unsafe_downgrade"], _))}
    as li2);
   li3;
  ]
  (* For older compiler 9.0 or earlier *)
    when li1.loc = li2.loc && li2.loc = li3.loc ->
    (* Not currently testable on 9.1.4 *)
    log 6
      "heuristic for JSX and compiler combined:\n\
       ~x becomes Js_OO.unsafe_downgrade(Props)#x\n\
       heuristic for: [Props, unsafe_downgrade, x], give loc of `x`";
    Some li3
  | [
   ({locType = Typed (_, _, LocalReference (_, Value))} as li1);
   ({locType = Typed (_, _, Definition (_, Value))} as li2);
  ] ->
    log 7
      "heuristic for JSX on type-annotated labeled (~arg:t):\n\
       (~arg:t) becomes Props#arg\n\
       Props has the location range of arg:t\n\
       arg has the location range of arg\n\
       heuristic for: [Props, arg], give loc of `arg`";
    if debug then Printf.printf "n1:%s n2:%s\n" (nameOf li1) (nameOf li2);
    Some li2
  | [li1; li2; li3] when li1.loc = li2.loc && li2.loc = li3.loc ->
    (* Not currently testable on 9.1.4 *)
    log 8
      "heuristic for JSX with at most one child\n\
       heuristic for: [makeProps, make, createElement], give the loc of `make` ";
    Some li2
  | [li1; li2; li3; li4]
    when li1.loc = li2.loc && li2.loc = li3.loc && li3.loc = li4.loc ->
    log 9
      "heuristic for JSX variadic, e.g. <C> {x} {y} </C>\n\
       heuristic for: [React.null, makeProps, make, createElementVariadic], \
       give the loc of `make`";
    if debug then
      Printf.printf "n1:%s n2:%s n3:%s n4:%s\n" (nameOf li1) (nameOf li2)
        (nameOf li3) (nameOf li4);
    Some li3
  | {locType = Typed (_, {desc = Tconstr (path, _, _)}, _)} :: li :: _
    when Utils.isUncurriedInternal path ->
    Some li
  | li :: _ -> Some li
  | _ -> None

let declaredForTip ~(stamps : Stamps.t) stamp (tip : Tip.t) =
  match tip with
  | Value ->
    Stamps.findValue stamps stamp
    |> Option.map (fun x -> {x with Declared.item = ()})
  | Field _ | Constructor _ | Type ->
    Stamps.findType stamps stamp
    |> Option.map (fun x -> {x with Declared.item = ()})
  | Module ->
    Stamps.findModule stamps stamp
    |> Option.map (fun x -> {x with Declared.item = ()})

let getField (file : File.t) stamp name =
  match Stamps.findType file.stamps stamp with
  | None -> None
  | Some {item = {kind}} -> (
    match kind with
    | Record fields -> fields |> List.find_opt (fun f -> f.fname.txt = name)
    | _ -> None)

let getConstructor (file : File.t) stamp name =
  match Stamps.findType file.stamps stamp with
  | None -> None
  | Some {item = {kind}} -> (
    match kind with
    | Variant constructors -> (
      match
        constructors
        |> List.find_opt (fun const -> const.Constructor.cname.txt = name)
      with
      | None -> None
      | Some const -> Some const)
    | _ -> None)

let exportedForTip ~env ~path ~package ~(tip : Tip.t) =
  match ResolvePath.resolvePath ~env ~path ~package with
  | None ->
    Log.log ("Cannot resolve path " ^ pathToString path);
    None
  | Some (env, name) -> (
    let kind =
      match tip with
      | Value -> Exported.Value
      | Field _ | Constructor _ | Type -> Exported.Type
      | Module -> Exported.Module
    in
    match Exported.find env.exported kind name with
    | None ->
      Log.log ("Exported not found for tip " ^ name ^ " > " ^ Tip.toString tip);
      None
    | Some stamp -> Some (env, name, stamp))

let definedForLoc ~file ~package locKind =
  let inner ~file stamp (tip : Tip.t) =
    match tip with
    | Constructor name -> (
      match getConstructor file stamp name with
      | None -> None
      | Some constructor ->
        Some (constructor.docstring, `Constructor constructor))
    | Field name ->
      Some
        ( (match getField file stamp name with
          | None -> []
          | Some field -> field.docstring),
          `Field )
    | _ -> (
      maybeLog
        ("Trying for declared " ^ Tip.toString tip ^ " " ^ string_of_int stamp
       ^ " in file " ^ Uri.toString file.uri);
      match declaredForTip ~stamps:file.stamps stamp tip with
      | None -> None
      | Some declared -> Some (declared.docstring, `Declared))
  in
  match locKind with
  | NotFound -> None
  | LocalReference (stamp, tip) | Definition (stamp, tip) ->
    inner ~file stamp tip
  | GlobalReference (moduleName, path, tip) -> (
    maybeLog ("Getting global " ^ moduleName);
    match ProcessCmt.fileForModule ~package moduleName with
    | None ->
      Log.log ("Cannot get module " ^ moduleName);
      None
    | Some file -> (
      let env = QueryEnv.fromFile file in
      match exportedForTip ~env ~path ~package ~tip with
      | None -> None
      | Some (env, name, stamp) -> (
        maybeLog ("Getting for " ^ string_of_int stamp ^ " in " ^ name);
        match inner ~file:env.file stamp tip with
        | None ->
          Log.log "could not get defined";
          None
        | Some res ->
          maybeLog "Yes!! got it";
          Some res)))

(** Find alternative declaration: from res in case of interface, or from resi in case of implementation  *)
let alternateDeclared ~(file : File.t) ~package (declared : _ Declared.t) tip =
  match Hashtbl.find_opt package.pathsForModule file.moduleName with
  | None -> None
  | Some paths -> (
    match paths with
    | IntfAndImpl {resi; res} -> (
      maybeLog
        ("alternateDeclared for " ^ file.moduleName ^ " has both resi and res");
      let alternateUri = if Uri.isInterface file.uri then res else resi in
      match Cmt.fullFromUri ~uri:(Uri.fromPath alternateUri) with
      | None -> None
      | Some {file; extra} -> (
        let env = QueryEnv.fromFile file in
        let path = ModulePath.toPath declared.modulePath declared.name.txt in
        maybeLog ("find declared for path " ^ pathToString path);
        let declaredOpt =
          match exportedForTip ~env ~path ~package ~tip with
          | None -> None
          | Some (_env, _name, stamp) ->
            declaredForTip ~stamps:file.stamps stamp tip
        in
        match declaredOpt with
        | None -> None
        | Some declared -> Some (file, extra, declared)))
    | _ ->
      maybeLog ("alternateDeclared for " ^ file.moduleName ^ " not found");

      None)

let rec resolveModuleReference ?(pathsSeen = []) ~file ~package
    (declared : Module.t Declared.t) =
  match declared.item with
  | Structure _ -> Some (file, Some declared)
  | Constraint (_moduleItem, moduleTypeItem) ->
    resolveModuleReference ~pathsSeen ~file ~package
      {declared with item = moduleTypeItem}
  | Ident path -> (
    let env = QueryEnv.fromFile file in
    match ResolvePath.fromCompilerPath ~env path with
    | NotFound -> None
    | Exported (env, name) -> (
      match Exported.find env.exported Exported.Module name with
      | None -> None
      | Some stamp -> (
        match Stamps.findModule env.file.stamps stamp with
        | None -> None
        | Some md -> Some (env.file, Some md)))
    | Global (moduleName, path) -> (
      match ProcessCmt.fileForModule ~package moduleName with
      | None -> None
      | Some file -> (
        let env = QueryEnv.fromFile file in
        match ResolvePath.resolvePath ~env ~package ~path with
        | None -> None
        | Some (env, name) -> (
          match Exported.find env.exported Exported.Module name with
          | None -> None
          | Some stamp -> (
            match Stamps.findModule env.file.stamps stamp with
            | None -> None
            | Some md -> Some (env.file, Some md)))))
    | Stamp stamp -> (
      match Stamps.findModule file.stamps stamp with
      | None -> None
      | Some ({item = Ident path} as md) when not (List.mem path pathsSeen) ->
        (* avoid possible infinite loops *)
        resolveModuleReference ~file ~package ~pathsSeen:(path :: pathsSeen) md
      | Some md -> Some (file, Some md))
    | GlobalMod name -> (
      match ProcessCmt.fileForModule ~package name with
      | None -> None
      | Some file -> Some (file, None)))

let validateLoc (loc : Location.t) (backup : Location.t) =
  if loc.loc_start.pos_cnum = -1 then
    if backup.loc_start.pos_cnum = -1 then
      {
        Location.loc_ghost = true;
        loc_start = {pos_cnum = 0; pos_lnum = 1; pos_bol = 0; pos_fname = ""};
        loc_end = {pos_cnum = 0; pos_lnum = 1; pos_bol = 0; pos_fname = ""};
      }
    else backup
  else loc

let resolveModuleDefinition ~(file : File.t) ~package stamp =
  match Stamps.findModule file.stamps stamp with
  | None -> None
  | Some md -> (
    match resolveModuleReference ~file ~package md with
    | None -> None
    | Some (file, declared) ->
      let loc =
        match declared with
        | None -> Uri.toTopLevelLoc file.uri
        | Some declared -> validateLoc declared.name.loc declared.extentLoc
      in
      Some (file.uri, loc))

let definition ~file ~package stamp (tip : Tip.t) =
  match tip with
  | Constructor name -> (
    match getConstructor file stamp name with
    | None -> None
    | Some constructor -> Some (file.uri, constructor.cname.loc))
  | Field name -> (
    match getField file stamp name with
    | None -> None
    | Some field -> Some (file.uri, field.fname.loc))
  | Module -> resolveModuleDefinition ~file ~package stamp
  | _ -> (
    match declaredForTip ~stamps:file.stamps stamp tip with
    | None -> None
    | Some declared ->
      let fileImpl, declaredImpl =
        match alternateDeclared ~package ~file declared tip with
        | Some (fileImpl, _extra, declaredImpl) when Uri.isInterface file.uri ->
          (fileImpl, declaredImpl)
        | _ -> (file, declared)
      in
      let loc = validateLoc declaredImpl.name.loc declaredImpl.extentLoc in
      let env = QueryEnv.fromFile fileImpl in
      let uri =
        ResolvePath.getSourceUri ~env ~package declaredImpl.modulePath
      in
      maybeLog ("Inner uri " ^ Uri.toString uri);
      Some (uri, loc))

let definitionForLocItem ~full:{file; package} locItem =
  match locItem.locType with
  | Typed (_, _, Definition (stamp, tip)) -> (
    maybeLog
      ("Typed Definition stamp:" ^ string_of_int stamp ^ " tip:"
     ^ Tip.toString tip);
    match declaredForTip ~stamps:file.stamps stamp tip with
    | None -> None
    | Some declared ->
      maybeLog ("Declared " ^ declared.name.txt);
      if declared.isExported then (
        maybeLog ("exported, looking for alternate " ^ file.moduleName);
        match alternateDeclared ~package ~file declared tip with
        | None -> None
        | Some (file, _extra, declared) ->
          let loc = validateLoc declared.name.loc declared.extentLoc in
          Some (file.uri, loc))
      else None)
  | Typed (_, _, NotFound)
  | LModule (NotFound | Definition (_, _))
  | TypeDefinition (_, _, _)
  | Constant _ ->
    None
  | TopLevelModule name -> (
    maybeLog ("Toplevel " ^ name);
    match Hashtbl.find_opt package.pathsForModule name with
    | None -> None
    | Some paths ->
      let uri = getUri paths in
      Some (uri, Uri.toTopLevelLoc uri))
  | LModule (LocalReference (stamp, tip))
  | Typed (_, _, LocalReference (stamp, tip)) ->
    maybeLog ("Local defn " ^ Tip.toString tip);
    definition ~file ~package stamp tip
  | LModule (GlobalReference (moduleName, path, tip))
  | Typed (_, _, GlobalReference (moduleName, path, tip)) -> (
    maybeLog
      ("Typed GlobalReference moduleName:" ^ moduleName ^ " path:"
     ^ pathToString path ^ " tip:" ^ Tip.toString tip);
    match ProcessCmt.fileForModule ~package moduleName with
    | None -> None
    | Some file -> (
      let env = QueryEnv.fromFile file in
      match exportedForTip ~env ~path ~package ~tip with
      | None -> None
      | Some (env, _name, stamp) ->
        (* oooh wht do I do if the stamp is inside a pseudo-file? *)
        maybeLog ("Got stamp " ^ string_of_int stamp);
        definition ~file:env.file ~package stamp tip))

let digConstructor ~env ~package path =
  match ResolvePath.resolveFromCompilerPath ~env ~package path with
  | NotFound -> None
  | Stamp stamp -> (
    match Stamps.findType env.file.stamps stamp with
    | None -> None
    | Some t -> Some (env, t))
  | Exported (env, name) -> (
    match Exported.find env.exported Exported.Type name with
    | None -> None
    | Some stamp -> (
      match Stamps.findType env.file.stamps stamp with
      | None -> None
      | Some t -> Some (env, t)))
  | _ -> None

let typeDefinitionForLocItem ~full:{file; package} locItem =
  match locItem.locType with
  | Constant _ | TopLevelModule _ | LModule _ -> None
  | TypeDefinition _ -> Some (file.uri, locItem.loc)
  | Typed (_, typ, _) -> (
    let env = QueryEnv.fromFile file in
    match Shared.digConstructor typ with
    | None -> None
    | Some path -> (
      match digConstructor ~env ~package path with
      | Some (env, declared) -> Some (env.file.uri, declared.item.decl.type_loc)
      | None -> None))

let isVisible (declared : _ Declared.t) =
  declared.isExported
  &&
  let rec loop (v : ModulePath.t) =
    match v with
    | File _ -> true
    | NotVisible -> false
    | IncludedModule (_, inner) -> loop inner
    | ExportedModule {modulePath = inner} -> loop inner
  in
  loop declared.modulePath

type references = {
  uri: Uri.t;
  locOpt: Location.t option; (* None: reference to a toplevel module *)
}

let forLocalStamp ~full:{file; extra; package} stamp (tip : Tip.t) =
  let env = QueryEnv.fromFile file in
  match
    match tip with
    | Constructor name ->
      getConstructor file stamp name
      |> Option.map (fun x -> x.Constructor.stamp)
    | Field name -> getField file stamp name |> Option.map (fun x -> x.stamp)
    | _ -> Some stamp
  with
  | None -> []
  | Some localStamp -> (
    match Hashtbl.find_opt extra.internalReferences localStamp with
    | None -> []
    | Some locs ->
      maybeLog ("Checking externals: " ^ string_of_int stamp);
      let externals =
        match declaredForTip ~stamps:env.file.stamps stamp tip with
        | None -> []
        | Some declared ->
          if isVisible declared then (
            let alternativeReferences =
              match alternateDeclared ~package ~file declared tip with
              | None -> []
              | Some (file, extra, {stamp}) -> (
                match
                  match tip with
                  | Constructor name ->
                    getConstructor file stamp name
                    |> Option.map (fun x -> x.Constructor.stamp)
                  | Field name ->
                    getField file stamp name |> Option.map (fun x -> x.stamp)
                  | _ -> Some stamp
                with
                | None -> []
                | Some localStamp -> (
                  match
                    Hashtbl.find_opt extra.internalReferences localStamp
                  with
                  | None -> []
                  | Some locs ->
                    locs
                    |> List.map (fun loc -> {uri = file.uri; locOpt = Some loc})
                  ))
              (* if this file has a corresponding interface or implementation file
                 also find the references in that file *)
            in
            let path =
              ModulePath.toPath declared.modulePath declared.name.txt
            in
            maybeLog ("Now checking path " ^ pathToString path);
            let thisModuleName = file.moduleName in
            let externals =
              package.projectFiles |> FileSet.elements
              |> List.filter (fun name -> name <> file.moduleName)
              |> List.map (fun moduleName ->
                     Cmt.fullsFromModule ~package ~moduleName
                     |> List.map (fun {file; extra} ->
                            match
                              Hashtbl.find_opt extra.externalReferences
                                thisModuleName
                            with
                            | None -> []
                            | Some refs ->
                              let locs =
                                refs
                                |> Utils.filterMap (fun (p, t, locs) ->
                                       if p = path && t = tip then Some locs
                                       else None)
                              in
                              locs
                              |> List.map (fun loc ->
                                     {uri = file.uri; locOpt = Some loc})))
              |> List.concat |> List.concat
            in
            alternativeReferences @ externals)
          else (
            maybeLog "Not visible";
            [])
      in
      List.append
        (locs |> List.map (fun loc -> {uri = file.uri; locOpt = Some loc}))
        externals)

let allReferencesForLocItem ~full:({file; package} as full) locItem =
  match locItem.locType with
  | TopLevelModule moduleName ->
    let otherModulesReferences =
      package.projectFiles |> FileSet.elements
      |> Utils.filterMap (fun name ->
             match ProcessCmt.fileForModule ~package name with
             | None -> None
             | Some file -> Cmt.fullFromUri ~uri:file.uri)
      |> List.map (fun full ->
             match Hashtbl.find_opt full.extra.fileReferences moduleName with
             | None -> []
             | Some locs ->
               locs |> LocationSet.elements
               |> List.map (fun loc ->
                      {
                        uri = Uri.fromPath loc.Location.loc_start.pos_fname;
                        locOpt = Some loc;
                      }))
      |> List.flatten
    in
    let targetModuleReferences =
      match Hashtbl.find_opt package.pathsForModule moduleName with
      | None -> []
      | Some paths ->
        let moduleSrcToRef src = {uri = Uri.fromPath src; locOpt = None} in
        getSrc paths |> List.map moduleSrcToRef
    in
    List.append targetModuleReferences otherModulesReferences
  | Typed (_, _, NotFound) | LModule NotFound | Constant _ -> []
  | TypeDefinition (_, _, stamp) -> forLocalStamp ~full stamp Type
  | Typed (_, _, (LocalReference (stamp, tip) | Definition (stamp, tip)))
  | LModule (LocalReference (stamp, tip) | Definition (stamp, tip)) ->
    maybeLog
      ("Finding references for " ^ Uri.toString file.uri ^ " and stamp "
     ^ string_of_int stamp ^ " and tip " ^ Tip.toString tip);
    forLocalStamp ~full stamp tip
  | LModule (GlobalReference (moduleName, path, tip))
  | Typed (_, _, GlobalReference (moduleName, path, tip)) -> (
    match ProcessCmt.fileForModule ~package moduleName with
    | None -> []
    | Some file -> (
      let env = QueryEnv.fromFile file in
      match exportedForTip ~env ~path ~package ~tip with
      | None -> []
      | Some (env, _name, stamp) -> (
        match Cmt.fullFromUri ~uri:env.file.uri with
        | None -> []
        | Some full ->
          maybeLog
            ("Finding references for (global) " ^ Uri.toString env.file.uri
           ^ " and stamp " ^ string_of_int stamp ^ " and tip "
           ^ Tip.toString tip);
          forLocalStamp ~full stamp tip)))
