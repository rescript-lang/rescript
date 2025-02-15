open SharedTypes

let showConstructor {Constructor.cname = {txt}; args; res} =
  txt
  ^ (match args with
    | Args [] -> ""
    | InlineRecord fields ->
      "({"
      ^ (fields
        |> List.map (fun (field : field) ->
               Printf.sprintf "%s%s: %s" field.fname.txt
                 (if field.optional then "?" else "")
                 (Shared.typeToString
                    (if field.optional then Utils.unwrapIfOption field.typ
                     else field.typ)))
        |> String.concat ", ")
      ^ "})"
    | Args args ->
      "("
      ^ (args
        |> List.map (fun (typ, _) -> typ |> Shared.typeToString)
        |> String.concat ", ")
      ^ ")")
  ^
  match res with
  | None -> ""
  | Some typ -> "\n" ^ (typ |> Shared.typeToString)

(* TODO: local opens *)
let resolveOpens ~env opens ~package =
  List.fold_left
    (fun previous path ->
      (* Finding an open, first trying to find it in previoulsly resolved opens *)
      let rec loop prev =
        match prev with
        | [] -> (
          match path with
          | [] | [_] -> previous
          | name :: path -> (
            match ProcessCmt.fileForModule ~package name with
            | None ->
              Log.log ("Could not get module " ^ name);
              previous (* TODO: warn? *)
            | Some file -> (
              match
                ResolvePath.resolvePath ~env:(QueryEnv.fromFile file) ~package
                  ~path
              with
              | None ->
                Log.log ("Could not resolve in " ^ name);
                previous
              | Some (env, _placeholder) -> previous @ [env])))
        | env :: rest -> (
          match ResolvePath.resolvePath ~env ~package ~path with
          | None -> loop rest
          | Some (env, _placeholder) -> previous @ [env])
      in
      Log.log ("resolving open " ^ pathToString path);
      match ResolvePath.resolvePath ~env ~package ~path with
      | None ->
        Log.log "Not local";
        loop previous
      | Some (env, _) ->
        Log.log "Was local";
        previous @ [env])
    (* loop(previous) *)
    [] opens

let completionForExporteds iterExported getDeclared ~prefix ~exact ~env
    ~namesUsed transformContents =
  let res = ref [] in
  iterExported (fun name stamp ->
      (* Log.log("checking exported: " ++ name); *)
      if Utils.checkName name ~prefix ~exact then
        match getDeclared stamp with
        | Some (declared : _ Declared.t)
          when not (Hashtbl.mem namesUsed declared.name.txt) ->
          Hashtbl.add namesUsed declared.name.txt ();
          res :=
            {
              (Completion.create declared.name.txt ~env
                 ~kind:(transformContents declared))
              with
              deprecated = declared.deprecated;
              docstring = declared.docstring;
            }
            :: !res
        | _ -> ());
  !res

let completionForExportedModules ~env ~prefix ~exact ~namesUsed =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Module)
    (Stamps.findModule env.file.stamps) ~prefix ~exact ~env ~namesUsed
    (fun declared ->
      Completion.Module
        {docstring = declared.docstring; module_ = declared.item})

let completionForExportedValues ~env ~prefix ~exact ~namesUsed =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Value)
    (Stamps.findValue env.file.stamps) ~prefix ~exact ~env ~namesUsed
    (fun declared -> Completion.Value declared.item)

let completionForExportedTypes ~env ~prefix ~exact ~namesUsed =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Type)
    (Stamps.findType env.file.stamps) ~prefix ~exact ~env ~namesUsed
    (fun declared -> Completion.Type declared.item)

let completionsForExportedConstructors ~(env : QueryEnv.t) ~prefix ~exact
    ~namesUsed =
  let res = ref [] in
  Exported.iter env.exported Exported.Type (fun _name stamp ->
      match Stamps.findType env.file.stamps stamp with
      | Some ({item = {kind = Type.Variant constructors}} as t) ->
        res :=
          (constructors
          |> List.filter (fun c ->
                 Utils.checkName c.Constructor.cname.txt ~prefix ~exact)
          |> Utils.filterMap (fun c ->
                 let name = c.Constructor.cname.txt in
                 if not (Hashtbl.mem namesUsed name) then
                   let () = Hashtbl.add namesUsed name () in
                   Some
                     (Completion.create name ~env ~docstring:c.docstring
                        ?deprecated:c.deprecated
                        ~kind:
                          (Completion.Constructor
                             (c, t.item.decl |> Shared.declToString t.name.txt)))
                 else None))
          @ !res
      | _ -> ());
  !res

let completionForExportedFields ~(env : QueryEnv.t) ~prefix ~exact ~namesUsed =
  let res = ref [] in
  Exported.iter env.exported Exported.Type (fun _name stamp ->
      match Stamps.findType env.file.stamps stamp with
      | Some ({item = {kind = Record fields}} as t) ->
        res :=
          (fields
          |> List.filter (fun f -> Utils.checkName f.fname.txt ~prefix ~exact)
          |> Utils.filterMap (fun f ->
                 let name = f.fname.txt in
                 if not (Hashtbl.mem namesUsed name) then
                   let () = Hashtbl.add namesUsed name () in
                   Some
                     (Completion.create name ~env ~docstring:f.docstring
                        ?deprecated:f.deprecated
                        ~kind:
                          (Completion.Field
                             (f, t.item.decl |> Shared.declToString t.name.txt)))
                 else None))
          @ !res
      | _ -> ());
  !res

let findModuleInScope ~env ~moduleName ~scope =
  let modulesTable = Hashtbl.create 10 in
  env.QueryEnv.file.stamps
  |> Stamps.iterModules (fun _ declared ->
         Hashtbl.replace modulesTable
           (declared.name.txt, declared.extentLoc |> Loc.start)
           declared);
  let result = ref None in
  let processModule name loc =
    if name = moduleName && !result = None then
      match Hashtbl.find_opt modulesTable (name, Loc.start loc) with
      | Some declared -> result := Some declared
      | None ->
        Log.log
          (Printf.sprintf "Module Not Found %s loc:%s\n" name (Loc.toString loc))
  in
  scope |> Scope.iterModulesBeforeFirstOpen processModule;
  scope |> Scope.iterModulesAfterFirstOpen processModule;
  !result

let resolvePathFromStamps ~(env : QueryEnv.t) ~package ~scope ~moduleName ~path
    =
  (* Log.log("Finding from stamps " ++ name); *)
  match findModuleInScope ~env ~moduleName ~scope with
  | None -> None
  | Some declared -> (
    (* Log.log("found it"); *)
    match ResolvePath.findInModule ~env declared.item path with
    | None -> None
    | Some res -> (
      match res with
      | `Local (env, name) -> Some (env, name)
      | `Global (moduleName, fullPath) -> (
        match ProcessCmt.fileForModule ~package moduleName with
        | None -> None
        | Some file ->
          ResolvePath.resolvePath ~env:(QueryEnv.fromFile file) ~path:fullPath
            ~package)))

let resolveModuleWithOpens ~opens ~package ~moduleName =
  let rec loop opens =
    match opens with
    | (env : QueryEnv.t) :: rest -> (
      Log.log ("Looking for env in " ^ Uri.toString env.file.uri);
      match ResolvePath.resolvePath ~env ~package ~path:[moduleName; ""] with
      | Some (env, _) -> Some env
      | None -> loop rest)
    | [] -> None
  in
  loop opens

let resolveFileModule ~moduleName ~package =
  Log.log ("Getting module " ^ moduleName);
  match ProcessCmt.fileForModule ~package moduleName with
  | None -> None
  | Some file ->
    Log.log "got it";
    let env = QueryEnv.fromFile file in
    Some env

let getEnvWithOpens ~scope ~(env : QueryEnv.t) ~package
    ~(opens : QueryEnv.t list) ~moduleName (path : string list) =
  (* TODO: handle interleaving of opens and local modules correctly *)
  match resolvePathFromStamps ~env ~scope ~moduleName ~path ~package with
  | Some x -> Some x
  | None -> (
    match resolveModuleWithOpens ~opens ~package ~moduleName with
    | Some env -> ResolvePath.resolvePath ~env ~package ~path
    | None -> (
      match resolveFileModule ~moduleName ~package with
      | None -> None
      | Some env -> ResolvePath.resolvePath ~env ~package ~path))

let rec expandTypeExpr ~env ~package typeExpr =
  match typeExpr |> Shared.digConstructor with
  | Some path -> (
    match References.digConstructor ~env ~package path with
    | None -> None
    | Some (env, {item = {decl = {type_manifest = Some t}}}) ->
      expandTypeExpr ~env ~package t
    | Some (_, {docstring; item}) -> Some (docstring, item))
  | None -> None

let kindToDocumentation ~env ~full ~currentDocstring name
    (kind : Completion.kind) =
  let docsFromKind =
    match kind with
    | ObjLabel _ | Label _ | FileModule _ | Snippet _ | FollowContextPath _ ->
      []
    | Module {docstring} -> docstring
    | Type {decl; name} ->
      [decl |> Shared.declToString name |> Markdown.codeBlock]
    | Value typ -> (
      match expandTypeExpr ~env ~package:full.package typ with
      | None -> []
      | Some (docstrings, {decl; name; kind}) ->
        docstrings
        @ [
            (match kind with
            | Record _ | Tuple _ | Variant _ ->
              Markdown.codeBlock (Shared.declToString name decl)
            | _ -> "");
          ])
    | Field ({typ; optional; docstring}, s) ->
      (* Handle optional fields. Checking for "?" is because sometimes optional
         fields are prefixed with "?" when completing, and at that point we don't
         need to _also_ add a "?" after the field name, as that looks weird. *)
      docstring
      @ [
          Markdown.codeBlock
            (if optional && Utils.startsWith name "?" = false then
               name ^ "?: "
               ^ (typ |> Utils.unwrapIfOption |> Shared.typeToString)
             else name ^ ": " ^ (typ |> Shared.typeToString));
          Markdown.codeBlock s;
        ]
    | Constructor (c, s) ->
      [Markdown.codeBlock (showConstructor c); Markdown.codeBlock s]
    | PolyvariantConstructor ({displayName; args}, s) ->
      [
        Markdown.codeBlock
          ("#" ^ displayName
          ^
          match args with
          | [] -> ""
          | typeExprs ->
            "("
            ^ (typeExprs
              |> List.map (fun typeExpr -> typeExpr |> Shared.typeToString)
              |> String.concat ", ")
            ^ ")");
        Markdown.codeBlock s;
      ]
    | ExtractedType (extractedType, _) ->
      [Markdown.codeBlock (TypeUtils.extractedTypeToString extractedType)]
  in
  currentDocstring @ docsFromKind
  |> List.filter (fun s -> s <> "")
  |> String.concat "\n\n"

let kindToDetail name (kind : Completion.kind) =
  match kind with
  | Type {name} -> "type " ^ name
  | Value typ -> typ |> Shared.typeToString
  | ObjLabel typ -> typ |> Shared.typeToString
  | Label typString -> typString
  | Module _ -> "module " ^ name
  | FileModule f -> "module " ^ f
  | Field ({typ; optional}, _) ->
    (* Handle optional fields. Checking for "?" is because sometimes optional
       fields are prefixed with "?" when completing, and at that point we don't
       need to _also_ add a "?" after the field name, as that looks weird. *)
    if optional && Utils.startsWith name "?" = false then
      typ |> Utils.unwrapIfOption |> Shared.typeToString
    else typ |> Shared.typeToString
  | Constructor (c, _) -> showConstructor c
  | PolyvariantConstructor ({displayName; args}, _) -> (
    "#" ^ displayName
    ^
    match args with
    | [] -> ""
    | typeExprs ->
      "("
      ^ (typeExprs
        |> List.map (fun typeExpr -> typeExpr |> Shared.typeToString)
        |> String.concat ", ")
      ^ ")")
  | Snippet s -> s
  | FollowContextPath _ -> ""
  | ExtractedType (extractedType, _) ->
    TypeUtils.extractedTypeToString ~nameOnly:true extractedType

let kindToData filePath (kind : Completion.kind) =
  match kind with
  | FileModule f -> Some [("modulePath", f); ("filePath", filePath)]
  | _ -> None

let findAllCompletions ~(env : QueryEnv.t) ~prefix ~exact ~namesUsed
    ~(completionContext : Completable.completionContext) =
  Log.log ("findAllCompletions uri:" ^ Uri.toString env.file.uri);
  match completionContext with
  | Value ->
    completionForExportedValues ~env ~prefix ~exact ~namesUsed
    @ completionsForExportedConstructors ~env ~prefix ~exact ~namesUsed
    @ completionForExportedModules ~env ~prefix ~exact ~namesUsed
  | Type ->
    completionForExportedTypes ~env ~prefix ~exact ~namesUsed
    @ completionForExportedModules ~env ~prefix ~exact ~namesUsed
  | Module -> completionForExportedModules ~env ~prefix ~exact ~namesUsed
  | Field ->
    completionForExportedFields ~env ~prefix ~exact ~namesUsed
    @ completionForExportedModules ~env ~prefix ~exact ~namesUsed
  | ValueOrField ->
    completionForExportedValues ~env ~prefix ~exact ~namesUsed
    @ completionForExportedFields ~env ~prefix ~exact ~namesUsed
    @ completionForExportedModules ~env ~prefix ~exact ~namesUsed

let processLocalValue name loc contextPath scope ~prefix ~exact ~env
    ~(localTables : LocalTables.t) =
  if Utils.checkName name ~prefix ~exact then
    match Hashtbl.find_opt localTables.valueTable (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create declared.name.txt ~env ~kind:(Value declared.item))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      if !Cfg.debugFollowCtxPath then
        Printf.printf "Completion Value Not Found %s loc:%s\n" name
          (Loc.toString loc);
      localTables.resultRev <-
        Completion.create name ~env
          ~kind:
            (match contextPath with
            | Some contextPath -> FollowContextPath (contextPath, scope)
            | None ->
              Value
                (Ctype.newconstr
                   (Path.Pident (Ident.create "Type Not Known"))
                   []))
        :: localTables.resultRev

let processLocalConstructor name loc ~prefix ~exact ~env
    ~(localTables : LocalTables.t) =
  if Utils.checkName name ~prefix ~exact then
    match
      Hashtbl.find_opt localTables.constructorTable (name, Loc.start loc)
    with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create declared.name.txt ~env
               ~kind:
                 (Constructor
                    ( declared.item,
                      snd declared.item.typeDecl
                      |> Shared.declToString (fst declared.item.typeDecl) )))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Constructor Not Found %s loc:%s\n" name
           (Loc.toString loc))

let processLocalType name loc ~prefix ~exact ~env ~(localTables : LocalTables.t)
    =
  if Utils.checkName name ~prefix ~exact then
    match Hashtbl.find_opt localTables.typesTable (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create declared.name.txt ~env ~kind:(Type declared.item))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Type Not Found %s loc:%s\n" name
           (Loc.toString loc))

let processLocalModule name loc ~prefix ~exact ~env
    ~(localTables : LocalTables.t) =
  if Utils.checkName name ~prefix ~exact then
    match Hashtbl.find_opt localTables.modulesTable (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create declared.name.txt ~env
               ~kind:
                 (Module
                    {docstring = declared.docstring; module_ = declared.item}))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Module Not Found %s loc:%s\n" name
           (Loc.toString loc))

let getItemsFromOpens ~opens ~localTables ~prefix ~exact ~completionContext =
  opens
  |> List.fold_left
       (fun results env ->
         let completionsFromThisOpen =
           findAllCompletions ~env ~prefix ~exact
             ~namesUsed:localTables.LocalTables.namesUsed ~completionContext
         in
         completionsFromThisOpen @ results)
       []

let findLocalCompletionsForValuesAndConstructors ~(localTables : LocalTables.t)
    ~env ~prefix ~exact ~opens ~scope =
  localTables |> LocalTables.populateValues ~env;
  localTables |> LocalTables.populateConstructors ~env;
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterValuesBeforeFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterConstructorsBeforeFirstOpen
       (processLocalConstructor ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact
      ~completionContext:Value
  in

  scope
  |> Scope.iterValuesAfterFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterConstructorsAfterFirstOpen
       (processLocalConstructor ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsForValues ~(localTables : LocalTables.t) ~env ~prefix
    ~exact ~opens ~scope =
  localTables |> LocalTables.populateValues ~env;
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterValuesBeforeFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact
      ~completionContext:Value
  in

  scope
  |> Scope.iterValuesAfterFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsForTypes ~(localTables : LocalTables.t) ~env ~prefix
    ~exact ~opens ~scope =
  localTables |> LocalTables.populateTypes ~env;
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterTypesBeforeFirstOpen
       (processLocalType ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact ~completionContext:Type
  in

  scope
  |> Scope.iterTypesAfterFirstOpen
       (processLocalType ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsForModules ~(localTables : LocalTables.t) ~env ~prefix
    ~exact ~opens ~scope =
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact
      ~completionContext:Module
  in

  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsWithOpens ~pos ~(env : QueryEnv.t) ~prefix ~exact ~opens
    ~scope ~(completionContext : Completable.completionContext) =
  (* TODO: handle arbitrary interleaving of opens and local bindings correctly *)
  Log.log
    ("findLocalCompletionsWithOpens uri:" ^ Uri.toString env.file.uri ^ " pos:"
   ^ Pos.toString pos);
  let localTables = LocalTables.create () in
  match completionContext with
  | Value | ValueOrField ->
    findLocalCompletionsForValuesAndConstructors ~localTables ~env ~prefix
      ~exact ~opens ~scope
  | Type ->
    findLocalCompletionsForTypes ~localTables ~env ~prefix ~exact ~opens ~scope
  | Module ->
    findLocalCompletionsForModules ~localTables ~env ~prefix ~exact ~opens
      ~scope
  | Field ->
    (* There's no local completion for fields *)
    []

let getComplementaryCompletionsForTypedValue ~opens ~allFiles ~scope ~env prefix
    =
  let exact = false in
  let localCompletionsWithOpens =
    let localTables = LocalTables.create () in
    findLocalCompletionsForValues ~localTables ~env ~prefix ~exact ~opens ~scope
  in
  let fileModules =
    allFiles |> FileSet.elements
    |> Utils.filterMap (fun name ->
           if
             Utils.checkName name ~prefix ~exact
             && not
                  (* TODO complete the namespaced name too *)
                  (Utils.fileNameHasUnallowedChars name)
           then
             Some
               (Completion.create name ~synthetic:true ~env
                  ~kind:(Completion.FileModule name))
           else None)
  in
  localCompletionsWithOpens @ fileModules

let getCompletionsForPath ~debug ~opens ~full ~pos ~exact ~scope
    ~completionContext ~env path =
  if debug then Printf.printf "Path %s\n" (path |> String.concat ".");
  let allFiles = allFilesInPackage full.package in
  match path with
  | [] -> []
  | [prefix] ->
    let localCompletionsWithOpens =
      findLocalCompletionsWithOpens ~pos ~env ~prefix ~exact ~opens ~scope
        ~completionContext
    in
    let fileModules =
      allFiles |> FileSet.elements
      |> Utils.filterMap (fun name ->
             if
               Utils.checkName name ~prefix ~exact
               && not
                    (* TODO complete the namespaced name too *)
                    (Utils.fileNameHasUnallowedChars name)
             then
               Some
                 (Completion.create name ~env ~kind:(Completion.FileModule name))
             else None)
    in
    localCompletionsWithOpens @ fileModules
  | moduleName :: path -> (
    Log.log ("Path " ^ pathToString path);
    match
      getEnvWithOpens ~scope ~env ~package:full.package ~opens ~moduleName path
    with
    | Some (env, prefix) ->
      Log.log "Got the env";
      let namesUsed = Hashtbl.create 10 in
      findAllCompletions ~env ~prefix ~exact ~namesUsed ~completionContext
    | None -> [])

(** Completions intended for piping, from a completion path. *)
let completionsForPipeFromCompletionPath ~envCompletionIsMadeFrom ~opens ~pos
    ~scope ~debug ~prefix ~env ~rawOpens ~full completionPath =
  let completionPathWithoutCurrentModule =
    TypeUtils.removeCurrentModuleIfNeeded ~envCompletionIsMadeFrom
      completionPath
  in
  let completionPathMinusOpens =
    TypeUtils.removeOpensFromCompletionPath ~rawOpens ~package:full.package
      completionPathWithoutCurrentModule
    |> String.concat "."
  in
  let completionName name =
    if completionPathMinusOpens = "" then name
    else completionPathMinusOpens ^ "." ^ name
  in
  let completions =
    completionPath @ [prefix]
    |> getCompletionsForPath ~debug ~completionContext:Value ~exact:false ~opens
         ~full ~pos ~env ~scope
  in
  let completions =
    completions
    |> List.map (fun (completion : Completion.t) ->
           {completion with name = completionName completion.name})
  in
  completions

let rec digToRecordFieldsForCompletion ~debug ~package ~opens ~full ~pos ~env
    ~scope path =
  match
    path
    |> getCompletionsForPath ~debug ~completionContext:Type ~exact:true ~opens
         ~full ~pos ~env ~scope
  with
  | {kind = Type {kind = Abstract (Some (p, _))}} :: _ ->
    (* This case happens when what we're looking for is a type alias.
       This is the case in newer rescript-react versions where
       ReactDOM.domProps is an alias for JsxEvent.t. *)
    let pathRev = p |> Utils.expandPath in
    pathRev |> List.rev
    |> digToRecordFieldsForCompletion ~debug ~package ~opens ~full ~pos ~env
         ~scope
  | {kind = Type {kind = Record fields}} :: _ -> Some fields
  | _ -> None

let mkItem ?data ?additionalTextEdits name ~kind ~detail ~deprecated ~docstring
    =
  let docContent =
    (match deprecated with
    | None -> ""
    | Some s -> "Deprecated: " ^ s ^ "\n\n")
    ^
    match docstring with
    | [] -> ""
    | _ :: _ -> docstring |> String.concat "\n"
  in
  let tags =
    match deprecated with
    | None -> []
    | Some _ -> [1 (* deprecated *)]
  in
  Protocol.
    {
      label = name;
      kind;
      tags;
      detail;
      documentation =
        (if docContent = "" then None
         else Some {kind = "markdown"; value = docContent});
      sortText = None;
      insertText = None;
      insertTextFormat = None;
      filterText = None;
      data;
      additionalTextEdits;
    }

let completionToItem
    {
      Completion.name;
      deprecated;
      docstring;
      kind;
      sortText;
      insertText;
      insertTextFormat;
      filterText;
      detail;
      env;
      additionalTextEdits;
    } ~full =
  let item =
    mkItem name ?additionalTextEdits
      ?data:(kindToData (full.file.uri |> Uri.toPath) kind)
      ~kind:(Completion.kindToInt kind)
      ~deprecated
      ~detail:
        (match detail with
        | None -> kindToDetail name kind
        | Some detail -> detail)
      ~docstring:
        (match
           kindToDocumentation ~currentDocstring:docstring ~full ~env name kind
         with
        | "" -> []
        | docstring -> [docstring])
  in
  {item with sortText; insertText; insertTextFormat; filterText}

let completionsGetTypeEnv = function
  | {Completion.kind = Value typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = ObjLabel typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = Field ({typ}, _); env} :: _ -> Some (typ, env)
  | _ -> None

type getCompletionsForContextPathMode = Regular | Pipe

let completionsGetCompletionType ~full completions =
  let firstNonSyntheticCompletion =
    List.find_opt (fun c -> not c.Completion.synthetic) completions
  in
  match firstNonSyntheticCompletion with
  | Some {Completion.kind = Value typ; env}
  | Some {Completion.kind = ObjLabel typ; env}
  | Some {Completion.kind = Field ({typ}, _); env} ->
    typ
    |> TypeUtils.extractType ~env ~package:full.package
    |> Option.map (fun (typ, _) -> (typ, env))
  | Some {Completion.kind = Type typ; env} -> (
    match TypeUtils.extractTypeFromResolvedType typ ~env ~full with
    | None -> None
    | Some extractedType -> Some (extractedType, env))
  | Some {Completion.kind = ExtractedType (typ, _); env} -> Some (typ, env)
  | _ -> None

let rec completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
    completions =
  let firstNonSyntheticCompletion =
    List.find_opt (fun c -> not c.Completion.synthetic) completions
  in
  match firstNonSyntheticCompletion with
  | Some
      ( {Completion.kind = Value typ; env}
      | {Completion.kind = ObjLabel typ; env}
      | {Completion.kind = Field ({typ}, _); env} ) ->
    Some (TypeExpr typ, env)
  | Some {Completion.kind = FollowContextPath (ctxPath, scope); env} ->
    ctxPath
    |> getCompletionsForContextPath ~debug ~full ~env ~exact:true ~opens
         ~rawOpens ~pos ~scope
    |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
  | Some {Completion.kind = Type typ; env} -> (
    match TypeUtils.extractTypeFromResolvedType typ ~env ~full with
    | None -> None
    | Some extractedType -> Some (ExtractedType extractedType, env))
  | Some {Completion.kind = ExtractedType (typ, _); env} ->
    Some (ExtractedType typ, env)
  | _ -> None

and completionsGetTypeEnv2 ~debug (completions : Completion.t list) ~full ~opens
    ~rawOpens ~pos =
  let firstNonSyntheticCompletion =
    List.find_opt (fun c -> not c.Completion.synthetic) completions
  in
  match firstNonSyntheticCompletion with
  | Some {Completion.kind = Value typ; env} -> Some (typ, env)
  | Some {Completion.kind = ObjLabel typ; env} -> Some (typ, env)
  | Some {Completion.kind = Field ({typ}, _); env} -> Some (typ, env)
  | Some {Completion.kind = FollowContextPath (ctxPath, scope); env} ->
    ctxPath
    |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
         ~exact:true ~scope
    |> completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens ~pos
  | _ -> None

and getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env ~exact
    ~scope ?(mode = Regular) contextPath =
  let envCompletionIsMadeFrom = env in
  if debug then
    Printf.printf "ContextPath %s\n"
      (Completable.contextPathToString contextPath);
  let package = full.package in
  match contextPath with
  | CPString ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPString";
    [Completion.create "dummy" ~env ~kind:(Completion.Value Predef.type_string)]
  | CPBool ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPBool";
    [Completion.create "dummy" ~env ~kind:(Completion.Value Predef.type_bool)]
  | CPInt ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPInt";
    [Completion.create "dummy" ~env ~kind:(Completion.Value Predef.type_int)]
  | CPFloat ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPFloat";
    [Completion.create "dummy" ~env ~kind:(Completion.Value Predef.type_float)]
  | CPArray None ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPArray (no payload)";
    [
      Completion.create "array" ~env
        ~kind:(Completion.Value (Ctype.newconstr Predef.path_array []));
    ]
  | CPArray (Some cp) -> (
    if Debug.verbose () then
      print_endline "[ctx_path]--> CPArray (with payload)";
    match mode with
    | Regular -> (
      match
        cp
        |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
             ~exact:true ~scope
        |> completionsGetCompletionType ~full
      with
      | None -> []
      | Some (typ, env) ->
        [
          Completion.create "dummy" ~env
            ~kind:
              (Completion.ExtractedType (Tarray (env, ExtractedType typ), `Type));
        ])
    | Pipe ->
      (* Pipe completion with array just needs to know that it's an array, not
         what inner type it has. *)
      [
        Completion.create "dummy" ~env
          ~kind:(Completion.Value (Ctype.newconstr Predef.path_array []));
      ])
  | CPOption cp -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CPOption";
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType ~full
    with
    | None -> []
    | Some (typ, env) ->
      [
        Completion.create "dummy" ~env
          ~kind:
            (Completion.ExtractedType (Toption (env, ExtractedType typ), `Type));
      ])
  | CPAwait cp -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CPAwait";
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType ~full
    with
    | Some (Tpromise (env, typ), _env) ->
      [Completion.create "dummy" ~env ~kind:(Completion.Value typ)]
    | _ -> [])
  | CPId {path; completionContext; loc} ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPId";
    (* Looks up the type of an identifier.

       Because of reasons we sometimes don't get enough type
       information when looking up identifiers where the type
       has type parameters. This in turn means less completions.

       There's a heuristic below that tries to look up the type
       of the ID in the usual way first. But if the type found
       still has uninstantiated type parameters, we check the
       location for the identifier from the compiler type artifacts.
       That type usually has the type params instantiated, if they are.
       This leads to better completion.

       However, we only do it in incremental type checking mode,
       because more type information is always available in that mode. *)
    let useTvarLookup = !Cfg.inIncrementalTypecheckingMode in
    let byPath =
      path
      |> getCompletionsForPath ~debug ~opens ~full ~pos ~exact
           ~completionContext ~env ~scope
    in
    let hasTvars =
      if useTvarLookup then
        match byPath with
        | [{kind = Value typ}] when TypeUtils.hasTvar typ -> true
        | _ -> false
      else false
    in
    let result =
      if hasTvars then
        let byLoc = TypeUtils.findTypeViaLoc loc ~full ~debug in
        match (byLoc, byPath) with
        | Some t, [({kind = Value _} as item)] -> [{item with kind = Value t}]
        | _ -> byPath
      else byPath
    in
    result
  | CPApply (cp, labels) -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CPApply";
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
    with
    | Some ((TypeExpr typ | ExtractedType (Tfunction {typ})), env) -> (
      let rec reconstructFunctionType args tRet =
        match args with
        | [] -> tRet
        | (label, tArg) :: rest ->
          let restType = reconstructFunctionType rest tRet in
          {typ with desc = Tarrow (label, tArg, restType, Cok, None)}
      in
      let rec processApply args labels =
        match (args, labels) with
        | _, [] -> args
        | _, label :: (_ :: _ as nextLabels) ->
          (* compute the application of the first label, then the next ones *)
          let args = processApply args [label] in
          processApply args nextLabels
        | (Asttypes.Noloc.Nolabel, _) :: nextArgs, [Asttypes.Noloc.Nolabel] ->
          nextArgs
        | ((Labelled _, _) as arg) :: nextArgs, [Nolabel] ->
          arg :: processApply nextArgs labels
        | (Optional _, _) :: nextArgs, [Nolabel] -> processApply nextArgs labels
        | ( (((Labelled s1 | Optional s1), _) as arg) :: nextArgs,
            [(Labelled s2 | Optional s2)] ) ->
          if s1 = s2 then nextArgs else arg :: processApply nextArgs labels
        | ((Nolabel, _) as arg) :: nextArgs, [(Labelled _ | Optional _)] ->
          arg :: processApply nextArgs labels
        | [], [(Nolabel | Labelled _ | Optional _)] ->
          (* should not happen, but just ignore extra arguments *) []
      in
      match TypeUtils.extractFunctionType ~env ~package typ with
      | args, tRet when args <> [] ->
        let args = processApply args labels in
        let retType = reconstructFunctionType args tRet in
        [Completion.create "dummy" ~env ~kind:(Completion.Value retType)]
      | _ -> [])
    | _ -> [])
  | CPField {contextPath = CPId {path; completionContext = Module}; fieldName}
    ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPField: M.field";
    (* M.field *)
    path @ [fieldName]
    |> getCompletionsForPath ~debug ~opens ~full ~pos ~exact
         ~completionContext:Field ~env ~scope
  | CPField {contextPath = cp; fieldName; posOfDot; exprLoc; inJsx} -> (
    if Debug.verbose () then print_endline "[dot_completion]--> Triggered";
    let completionsFromCtxPath =
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
    in
    let mainTypeCompletionEnv =
      completionsFromCtxPath
      |> completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens ~pos
    in
    match mainTypeCompletionEnv with
    | None ->
      if Debug.verbose () then
        Printf.printf
          "[dot_completion] Could not extract main type completion env.\n";
      []
    | Some (typ, env) ->
      let fieldCompletions =
        DotCompletionUtils.fieldCompletionsForDotCompletion typ ~env ~package
          ~prefix:fieldName ?posOfDot ~exact
      in
      (* Get additional completions acting as if this field completion was actually a pipe completion. *)
      let cpAsPipeCompletion =
        Completable.CPPipe
          {
            synthetic = true;
            contextPath =
              (match cp with
              | CPApply (c, args) -> CPApply (c, args @ [Asttypes.Noloc.Nolabel])
              | CPId _ when TypeUtils.isFunctionType ~env ~package typ ->
                CPApply (cp, [Asttypes.Noloc.Nolabel])
              | _ -> cp);
            id = fieldName;
            inJsx;
            lhsLoc = exprLoc;
          }
      in
      let pipeCompletions =
        cpAsPipeCompletion
        |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos
             ~env:envCompletionIsMadeFrom ~exact ~scope
        |> List.filter_map (fun c ->
               TypeUtils.transformCompletionToPipeCompletion ~synthetic:true
                 ~env ?posOfDot c)
      in
      fieldCompletions @ pipeCompletions)
  | CPObj (cp, label) -> (
    (* TODO: Also needs to support ExtractedType *)
    if Debug.verbose () then print_endline "[ctx_path]--> CPObj";
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens ~pos
    with
    | Some (typ, env) -> (
      match typ |> TypeUtils.extractObjectType ~env ~package with
      | Some (env, tObj) ->
        tObj |> TypeUtils.getObjFields
        |> Utils.filterMap (fun (field, typ) ->
               if Utils.checkName field ~prefix:label ~exact then
                 Some
                   (Completion.create field ~env ~kind:(Completion.ObjLabel typ))
               else None)
      | None -> [])
    | None -> [])
  | CPPipe {contextPath = cp; id = prefix; lhsLoc; inJsx; synthetic} -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CPPipe";
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope ~mode:Pipe
      |> completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens ~pos
    with
    | None ->
      if Debug.verbose () then
        print_endline "[CPPipe]--> Could not resolve type env";
      []
    | Some (typ, env) -> (
      let env, typ =
        typ
        |> TypeUtils.resolveTypeForPipeCompletion ~env ~package:full.package
             ~full ~lhsLoc
      in
      let mainTypeId = TypeUtils.findRootTypeId ~full ~env typ in
      let typePath = TypeUtils.pathFromTypeExpr typ in
      match mainTypeId with
      | None ->
        if Debug.verbose () then
          Printf.printf
            "[pipe_completion] Could not find mainTypeId. Aborting pipe \
             completions.\n";
        []
      | Some mainTypeId ->
        if Debug.verbose () then
          Printf.printf "[pipe_completion] mainTypeId: %s\n" mainTypeId;
        let pipeCompletions =
          (* We now need a completion path from where to look up the module for our dot completion type.
              This is from where we pull all of the functions we want to complete for the pipe.

              A completion path here could be one of two things:
              1. A module path to the main module for the type we've found
              2. A module path to a builtin module, like `Int` for `int`, or `Array` for `array`

             The below code will deliberately _not_ dig into type aliases for the main type when we're looking
             for what _module_ to complete from. This is because you should be able to control where completions
             come from even if your type is an alias.
          *)
          let completeAsBuiltin =
            match typePath with
            | Some t -> TypeUtils.completionPathFromMaybeBuiltin t
            | None -> None
          in
          let completionPath =
            match (completeAsBuiltin, typePath) with
            | Some completionPathForBuiltin, _ ->
              Some (false, completionPathForBuiltin)
            | _, Some p -> (
              (* If this isn't a builtin, but we have a path, we try to resolve the
                 module path relative to the env we're completing from. This ensures that
                 what we get here is a module path we can find completions for regardless of
                 of the current scope for the position we're at.*)
              match
                TypeUtils.getModulePathRelativeToEnv ~debug
                  ~env:envCompletionIsMadeFrom ~envFromItem:env
                  (Utils.expandPath p)
              with
              | None -> Some (true, [env.file.moduleName])
              | Some p -> Some (false, p))
            | _ -> None
          in
          match completionPath with
          | None -> []
          | Some (isFromCurrentModule, completionPath) ->
            completionsForPipeFromCompletionPath ~envCompletionIsMadeFrom ~opens
              ~pos ~scope ~debug ~prefix ~env ~rawOpens ~full completionPath
            |> TypeUtils.filterPipeableFunctions ~env ~full ~synthetic
                 ~targetTypeId:mainTypeId
            |> List.filter (fun (c : Completion.t) ->
                   (* If we're completing from the current module then we need to care about scope.
                      This is automatically taken care of in other cases. *)
                   if isFromCurrentModule then
                     match c.kind with
                     | Value _ ->
                       scope
                       |> List.find_opt (fun (item : ScopeTypes.item) ->
                              match item with
                              | Value (scopeItemName, _, _, _) ->
                                scopeItemName = c.name
                              | _ -> false)
                       |> Option.is_some
                     | _ -> false
                   else true)
        in
        (* Extra completions can be drawn from the @editor.completeFrom attribute. Here we
           find and add those completions as well. *)
        let extraCompletions =
          TypeUtils.getExtraModulesToCompleteFromForType ~env ~full typ
          |> List.map (fun completionPath ->
                 completionsForPipeFromCompletionPath ~envCompletionIsMadeFrom
                   ~opens ~pos ~scope ~debug ~prefix ~env ~rawOpens ~full
                   completionPath)
          |> List.flatten
          |> TypeUtils.filterPipeableFunctions ~synthetic:true ~env ~full
               ~targetTypeId:mainTypeId
        in
        (* Add JSX completion items if we're in a JSX context. *)
        let jsxCompletions =
          if inJsx then
            PipeCompletionUtils.addJsxCompletionItems ~env ~mainTypeId ~prefix
              ~full ~rawOpens typ
          else []
        in
        jsxCompletions @ pipeCompletions @ extraCompletions))
  | CTuple ctxPaths ->
    if Debug.verbose () then print_endline "[ctx_path]--> CTuple";
    (* Turn a list of context paths into a list of type expressions. *)
    let typeExrps =
      ctxPaths
      |> List.map (fun contextPath ->
             contextPath
             |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos
                  ~env ~exact:true ~scope)
      |> List.filter_map (fun completionItems ->
             match completionItems with
             | {Completion.kind = Value typ} :: _ -> Some typ
             | _ -> None)
    in
    if List.length ctxPaths = List.length typeExrps then
      [
        Completion.create "dummy" ~env
          ~kind:(Completion.Value (Ctype.newty (Ttuple typeExrps)));
      ]
    else []
  | CJsxPropValue {pathToComponent; propName; emptyJsxPropNameHint} -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CJsxPropValue";
    let findTypeOfValue path =
      path
      |> getCompletionsForPath ~debug ~completionContext:Value ~exact:true
           ~opens ~full ~pos ~env ~scope
      |> completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens ~pos
    in
    let lowercaseComponent =
      match pathToComponent with
      | [elName] when Char.lowercase_ascii elName.[0] = elName.[0] -> true
      | _ -> false
    in
    (* TODO(env-stuff) Does this need to potentially be instantiated with type args too? *)
    let labels =
      if lowercaseComponent then
        let rec digToTypeForCompletion path =
          match
            path
            |> getCompletionsForPath ~debug ~completionContext:Type ~exact:true
                 ~opens ~full ~pos ~env ~scope
          with
          | {kind = Type {kind = Abstract (Some (p, _))}} :: _ ->
            (* This case happens when what we're looking for is a type alias.
               This is the case in newer rescript-react versions where
               ReactDOM.domProps is an alias for JsxEvent.t. *)
            let pathRev = p |> Utils.expandPath in
            pathRev |> List.rev |> digToTypeForCompletion
          | {kind = Type {kind = Record fields}} :: _ ->
            fields |> List.map (fun f -> (f.fname.txt, f.typ, env))
          | _ -> []
        in
        TypeUtils.pathToElementProps package |> digToTypeForCompletion
      else
        CompletionJsx.getJsxLabels ~componentPath:pathToComponent
          ~findTypeOfValue ~package
    in
    (* We have a heuristic that kicks in when completing empty prop expressions in the middle of a JSX element,
       like <SomeComp firstProp=test second=<com> third=123 />.
       The parser turns that broken JSX into: <SomeComp firstProp=test second=<com>third />, 123.

       So, we use a heuristic that covers this scenario by picking up on the cursor being between
       the prop name and the prop expression, and the prop expression being an ident that's a
       _valid prop name_ for that JSX element.

       This works because the ident itself will always be the next prop name (since that's what the
       parser eats). So, we do a simple lookup of that hint here if it exists, to make sure the hint
       is indeed a valid label for this JSX element. *)
    let emptyJsxPropNameHintIsCorrect =
      match emptyJsxPropNameHint with
      | Some identName when identName != propName ->
        labels
        |> List.find_opt (fun (f, _, _) -> f = identName)
        |> Option.is_some
      | Some _ -> false
      | None -> true
    in
    let targetLabel =
      if emptyJsxPropNameHintIsCorrect then
        labels |> List.find_opt (fun (f, _, _) -> f = propName)
      else None
    in
    match targetLabel with
    | None -> []
    | Some (_, typ, env) ->
      [
        Completion.create "dummy" ~env
          ~kind:(Completion.Value (Utils.unwrapIfOption typ));
      ])
  | CArgument {functionContextPath; argumentLabel} -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CArgument";
    if Debug.verbose () then
      Printf.printf "--> function argument: %s\n"
        (match argumentLabel with
        | Labelled n | Optional n -> n
        | Unlabelled {argumentPosition} -> "$" ^ string_of_int argumentPosition);

    let labels, env =
      match
        functionContextPath
        |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
             ~exact:true ~scope
        |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
      with
      | Some ((TypeExpr typ | ExtractedType (Tfunction {typ})), env) ->
        if Debug.verbose () then print_endline "--> found function type";
        (typ |> TypeUtils.getArgs ~full ~env, env)
      | _ ->
        if Debug.verbose () then
          print_endline "--> could not find function type";
        ([], env)
    in
    let targetLabel =
      labels
      |> List.find_opt (fun (label, _) ->
             match (argumentLabel, label) with
             | ( Unlabelled {argumentPosition = pos1},
                 Completable.Unlabelled {argumentPosition = pos2} ) ->
               pos1 = pos2
             | ( (Labelled name1 | Optional name1),
                 (Labelled name2 | Optional name2) ) ->
               name1 = name2
             | _ -> false)
    in
    let expandOption =
      match targetLabel with
      | None | Some ((Unlabelled _ | Labelled _), _) -> false
      | Some (Optional _, _) -> true
    in
    match targetLabel with
    | None ->
      if Debug.verbose () then
        print_endline "--> could not look up function argument";
      []
    | Some (_, typ) ->
      if Debug.verbose () then print_endline "--> found function argument!";
      [
        Completion.create "dummy" ~env
          ~kind:
            (Completion.Value
               (if expandOption then Utils.unwrapIfOption typ else typ));
      ])
  | CPatternPath {rootCtxPath; nested} -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CPatternPath";
    (* TODO(env-stuff) Get rid of innerType etc *)
    match
      rootCtxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
    with
    | Some (typ, env) -> (
      match typ |> TypeUtils.resolveNestedPatternPath ~env ~full ~nested with
      | Some (typ, env) ->
        [Completion.create "dummy" ~env ~kind:(kindFromInnerType typ)]
      | None -> [])
    | None -> [])
  | CTypeAtPos loc -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CTypeAtPos";
    match TypeUtils.findTypeViaLoc loc ~full ~debug with
    | None -> []
    | Some typExpr -> [Completion.create "dummy" ~env ~kind:(Value typExpr)])

let getOpens ~debug ~rawOpens ~package ~env =
  if debug && rawOpens <> [] then
    Printf.printf "%s\n"
      ("Raw opens: "
      ^ string_of_int (List.length rawOpens)
      ^ " "
      ^ String.concat " ... " (rawOpens |> List.map pathToString));
  let packageOpens = package.opens in
  if debug && packageOpens <> [] then
    Printf.printf "%s\n"
      ("Package opens "
      ^ String.concat " " (packageOpens |> List.map (fun p -> p |> pathToString))
      );
  let resolvedOpens =
    resolveOpens ~env (List.rev (rawOpens @ packageOpens)) ~package
  in
  if debug && resolvedOpens <> [] then
    Printf.printf "%s\n"
      ("Resolved opens "
      ^ string_of_int (List.length resolvedOpens)
      ^ " "
      ^ String.concat " "
          (resolvedOpens |> List.map (fun (e : QueryEnv.t) -> e.file.moduleName))
      );
  (* Last open takes priority *)
  List.rev resolvedOpens

let filterItems items ~prefix =
  if prefix = "" then items
  else
    items
    |> List.filter (fun (item : Completion.t) ->
           Utils.startsWith item.name prefix)

type completionMode = Pattern of Completable.patternMode | Expression

let emptyCase ~mode num =
  match mode with
  | Expression -> "$" ^ string_of_int (num - 1)
  | Pattern _ -> "${" ^ string_of_int num ^ ":_}"

let printConstructorArgs ~mode ~asSnippet argsLen =
  let args = ref [] in
  for argNum = 1 to argsLen do
    args :=
      !args
      @ [
          (match (asSnippet, argsLen) with
          | true, l when l > 1 -> Printf.sprintf "${%i:_}" argNum
          | true, l when l > 0 -> emptyCase ~mode argNum
          | _ -> "_");
        ]
  done;
  if List.length !args > 0 then "(" ^ (!args |> String.concat ", ") ^ ")"
  else ""

let rec completeTypedValue ?(typeArgContext : typeArgContext option) ~rawOpens
    ~full ~prefix ~completionContext ~mode (t : SharedTypes.completionType) =
  let emptyCase = emptyCase ~mode in
  let printConstructorArgs = printConstructorArgs ~mode in
  let create = Completion.create ?typeArgContext in
  match t with
  | TtypeT {env; path} when mode = Expression ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> TtypeT (Expression)";
    (* Find all values in the module with type t *)
    let valueWithTypeT t =
      match t.Types.desc with
      | Tconstr (Pident {name = "t"}, [], _) -> true
      | _ -> false
    in
    (* Find all functions in the module that returns type t *)
    let rec fnReturnsTypeT t =
      match t.Types.desc with
      | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> fnReturnsTypeT t1
      | Tarrow _ -> (
        match TypeUtils.extractFunctionType ~env ~package:full.package t with
        | ( (Nolabel, {desc = Tconstr (Path.Pident {name = "t"}, _, _)}) :: _,
            {desc = Tconstr (Path.Pident {name = "t"}, _, _)} ) ->
          (* Filter out functions that take type t first. These are often
             @send style functions that we don't want to have here because
             they usually aren't meant to create a type t from scratch. *)
          false
        | _args, {desc = Tconstr (Path.Pident {name = "t"}, _, _)} -> true
        | _ -> false)
      | _ -> false
    in
    let getCompletionName exportedValueName =
      let fnNname =
        TypeUtils.getModulePathRelativeToEnv ~debug:false
          ~env:(QueryEnv.fromFile full.file)
          ~envFromItem:env (Utils.expandPath path)
      in
      match fnNname with
      | None -> None
      | Some base ->
        let base =
          TypeUtils.removeOpensFromCompletionPath ~rawOpens
            ~package:full.package base
        in
        Some ((base |> String.concat ".") ^ "." ^ exportedValueName)
    in
    let getExportedValueCompletion name (declared : Types.type_expr Declared.t)
        =
      let typeExpr = declared.item in
      if valueWithTypeT typeExpr then
        getCompletionName name
        |> Option.map (fun name ->
               create name ~includesSnippets:true ~insertText:name
                 ~kind:(Value typeExpr) ~env)
      else if fnReturnsTypeT typeExpr then
        getCompletionName name
        |> Option.map (fun name ->
               create
                 (Printf.sprintf "%s()" name)
                 ~includesSnippets:true ~insertText:(name ^ "($0)")
                 ~kind:(Value typeExpr) ~env)
      else None
    in
    let completionItems =
      Hashtbl.fold
        (fun name stamp all ->
          match Stamps.findValue env.file.stamps stamp with
          | None -> all
          | Some declaredTypeExpr -> (
            match getExportedValueCompletion name declaredTypeExpr with
            | None -> all
            | Some completion -> completion :: all))
        env.exported.values_ []
    in

    (* Special casing for things where we want extra things in the completions *)
    let completionItems =
      match path with
      | Pdot (Pdot (Pident {name = "Js"}, "Re", _), "t", _)
      | Pdot (Pident {name = "RegExp"}, "t", _) ->
        (* regexps *)
        create "/<regexp>/g" ~insertText:"/$0/g" ~includesSnippets:true
          ~kind:(Label "Regular expression") ~env
        :: completionItems
      | _ -> completionItems
    in
    completionItems
  | Tbool env ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tbool";
    [
      create "true" ~kind:(Label "bool") ~env;
      create "false" ~kind:(Label "bool") ~env;
    ]
    |> filterItems ~prefix
  | TtypeT {env; path} ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> TtypeT (Pattern)";
    (* This is in patterns. Emit an alias/binding with the module name as a value name. *)
    if prefix <> "" then []
    else
      let moduleName =
        match path |> Utils.expandPath with
        | _t :: moduleName :: _rest -> String.uncapitalize_ascii moduleName
        | _ -> "value"
      in
      [
        create moduleName ~kind:(Label moduleName) ~env
          ~insertText:("${0:" ^ moduleName ^ "}")
          ~includesSnippets:true;
      ]
  | Tvariant {env; constructors; variantDecl; variantName} ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tvariant";
    constructors
    |> List.map (fun (constructor : Constructor.t) ->
           let numArgs =
             match constructor.args with
             | InlineRecord _ -> 1
             | Args args -> List.length args
           in
           create ?deprecated:constructor.deprecated ~includesSnippets:true
             (constructor.cname.txt
             ^ printConstructorArgs numArgs ~asSnippet:false)
             ~insertText:
               (constructor.cname.txt
               ^ printConstructorArgs numArgs ~asSnippet:true)
             ~kind:
               (Constructor
                  (constructor, variantDecl |> Shared.declToString variantName))
             ~env)
    |> filterItems ~prefix
  | Tpolyvariant {env; constructors; typeExpr} ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> Tpolyvariant";
    constructors
    |> List.map (fun (constructor : polyVariantConstructor) ->
           create
             ("#" ^ constructor.displayName
             ^ printConstructorArgs
                 (List.length constructor.args)
                 ~asSnippet:false)
             ~includesSnippets:true
             ~insertText:
               ((if Utils.startsWith prefix "#" then "" else "#")
               ^ constructor.displayName
               ^ printConstructorArgs
                   (List.length constructor.args)
                   ~asSnippet:true)
             ~kind:
               (PolyvariantConstructor
                  (constructor, typeExpr |> Shared.typeToString))
             ~env)
    |> filterItems
         ~prefix:(if Utils.startsWith prefix "#" then prefix else "#" ^ prefix)
  | Toption (env, t) ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Toption";
    let innerType =
      match t with
      | ExtractedType t -> Some (t, None)
      | TypeExpr t -> t |> TypeUtils.extractType ~env ~package:full.package
    in
    let expandedCompletions =
      match innerType with
      | None -> []
      | Some (innerType, _typeArgsContext) ->
        innerType
        |> completeTypedValue ~rawOpens ~full ~prefix ~completionContext ~mode
        |> List.map (fun (c : Completion.t) ->
               {
                 c with
                 name = "Some(" ^ c.name ^ ")";
                 sortText = None;
                 insertText =
                   (match c.insertText with
                   | None -> None
                   | Some insertText -> Some ("Some(" ^ insertText ^ ")"));
               })
    in
    let noneCase = Completion.create "None" ~kind:(kindFromInnerType t) ~env in
    let someAnyCase =
      create "Some(_)" ~includesSnippets:true ~kind:(kindFromInnerType t) ~env
        ~insertText:(Printf.sprintf "Some(%s)" (emptyCase 1))
    in
    let completions =
      match completionContext with
      | Some (Completable.CameFromRecordField fieldName) ->
        [
          create
            ("Some(" ^ fieldName ^ ")")
            ~includesSnippets:true ~kind:(kindFromInnerType t) ~env
            ~insertText:("Some(" ^ fieldName ^ ")$0");
          someAnyCase;
          noneCase;
        ]
      | _ -> [noneCase; someAnyCase]
    in
    completions @ expandedCompletions |> filterItems ~prefix
  | Tresult {env; okType; errorType} ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tresult";
    let okInnerType =
      okType |> TypeUtils.extractType ~env ~package:full.package
    in
    let errorInnerType =
      errorType |> TypeUtils.extractType ~env ~package:full.package
    in
    let expandedOkCompletions =
      match okInnerType with
      | None -> []
      | Some (innerType, _) ->
        innerType
        |> completeTypedValue ~rawOpens ~full ~prefix ~completionContext ~mode
        |> List.map (fun (c : Completion.t) ->
               {
                 c with
                 name = "Ok(" ^ c.name ^ ")";
                 sortText = None;
                 insertText =
                   (match c.insertText with
                   | None -> None
                   | Some insertText -> Some ("Ok(" ^ insertText ^ ")"));
               })
    in
    let expandedErrorCompletions =
      match errorInnerType with
      | None -> []
      | Some (innerType, _) ->
        innerType
        |> completeTypedValue ~rawOpens ~full ~prefix ~completionContext ~mode
        |> List.map (fun (c : Completion.t) ->
               {
                 c with
                 name = "Error(" ^ c.name ^ ")";
                 sortText = None;
                 insertText =
                   (match c.insertText with
                   | None -> None
                   | Some insertText -> Some ("Error(" ^ insertText ^ ")"));
               })
    in
    let okAnyCase =
      create "Ok(_)" ~includesSnippets:true ~kind:(Value okType) ~env
        ~insertText:(Printf.sprintf "Ok(%s)" (emptyCase 1))
    in
    let errorAnyCase =
      create "Error(_)" ~includesSnippets:true ~kind:(Value errorType) ~env
        ~insertText:(Printf.sprintf "Error(%s)" (emptyCase 1))
    in
    let completions =
      match completionContext with
      | Some (Completable.CameFromRecordField fieldName) ->
        [
          create
            ("Ok(" ^ fieldName ^ ")")
            ~includesSnippets:true ~kind:(Value okType) ~env
            ~insertText:("Ok(" ^ fieldName ^ ")$0");
          okAnyCase;
          errorAnyCase;
        ]
      | _ -> [okAnyCase; errorAnyCase]
    in
    completions @ expandedOkCompletions @ expandedErrorCompletions
    |> filterItems ~prefix
  | Tuple (env, exprs, typ) ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tuple";
    let numExprs = List.length exprs in
    [
      create
        (printConstructorArgs numExprs ~asSnippet:false)
        ~includesSnippets:true
        ~insertText:(printConstructorArgs numExprs ~asSnippet:true)
        ~kind:(Value typ) ~env;
    ]
  | Trecord {env; fields} as extractedType -> (
    if Debug.verbose () then print_endline "[complete_typed_value]--> Trecord";
    (* As we're completing for a record, we'll need a hint (completionContext)
       here to figure out whether we should complete for a record field, or
       the record body itself. *)
    match completionContext with
    | Some (Completable.RecordField {seenFields}) ->
      fields
      |> List.filter (fun (field : field) ->
             List.mem field.fname.txt seenFields = false)
      |> List.map (fun (field : field) ->
             match (field.optional, mode) with
             | true, Pattern Destructuring ->
               create ("?" ^ field.fname.txt) ?deprecated:field.deprecated
                 ~docstring:
                   [
                     field.fname.txt
                     ^ " is an optional field, and needs to be destructured \
                        using '?'.";
                   ]
                 ~kind:
                   (Field (field, TypeUtils.extractedTypeToString extractedType))
                 ~env
             | _ ->
               create field.fname.txt ?deprecated:field.deprecated
                 ~kind:
                   (Field (field, TypeUtils.extractedTypeToString extractedType))
                 ~env)
      |> filterItems ~prefix
    | _ ->
      if prefix = "" then
        [
          create "{}" ~includesSnippets:true ~insertText:"{$0}" ~sortText:"A"
            ~kind:
              (ExtractedType
                 ( extractedType,
                   match mode with
                   | Pattern _ -> `Type
                   | Expression -> `Value ))
            ~env;
        ]
      else [])
  | TinlineRecord {env; fields} -> (
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> TinlineRecord";
    match completionContext with
    | Some (Completable.RecordField {seenFields}) ->
      fields
      |> List.filter (fun (field : field) ->
             List.mem field.fname.txt seenFields = false)
      |> List.map (fun (field : field) ->
             create field.fname.txt ~kind:(Label "Inline record")
               ?deprecated:field.deprecated ~env)
      |> filterItems ~prefix
    | _ ->
      if prefix = "" then
        [
          create "{}" ~includesSnippets:true ~insertText:"{$0}" ~sortText:"A"
            ~kind:(Label "Inline record") ~env;
        ]
      else [])
  | Tarray (env, typ) ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tarray";
    if prefix = "" then
      [
        create "[]" ~includesSnippets:true ~insertText:"[$0]" ~sortText:"A"
          ~kind:
            (match typ with
            | ExtractedType typ ->
              ExtractedType
                ( typ,
                  match mode with
                  | Pattern _ -> `Type
                  | Expression -> `Value )
            | TypeExpr typ -> Value typ)
          ~env;
      ]
    else []
  | Tstring env ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tstring";
    if prefix = "" then
      [
        create "\"\"" ~includesSnippets:true ~insertText:"\"$0\"" ~sortText:"A"
          ~kind:(Value Predef.type_string) ~env;
      ]
    else []
  | Tfunction {env; typ; args; returnType} when prefix = "" && mode = Expression
    ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> Tfunction #1";
    let shouldPrintAsUncurried = false in
    let mkFnArgs ~asSnippet =
      match args with
      | [(Nolabel, argTyp)] when TypeUtils.typeIsUnit argTyp ->
        if shouldPrintAsUncurried then "(. )" else "()"
      | [(Nolabel, argTyp)] ->
        let varName =
          CompletionExpressions.prettyPrintFnTemplateArgName ~env ~full argTyp
        in
        let argsText = if asSnippet then "${1:" ^ varName ^ "}" else varName in
        if shouldPrintAsUncurried then "(. " ^ argsText ^ ")" else argsText
      | _ ->
        let currentUnlabelledIndex = ref 0 in
        let argsText =
          args
          |> List.map (fun ((label, typ) : typedFnArg) ->
                 match label with
                 | Optional name -> "~" ^ name ^ "=?"
                 | Labelled name -> "~" ^ name
                 | Nolabel ->
                   if TypeUtils.typeIsUnit typ then "()"
                   else (
                     currentUnlabelledIndex := !currentUnlabelledIndex + 1;
                     let num = !currentUnlabelledIndex in
                     let varName =
                       CompletionExpressions.prettyPrintFnTemplateArgName
                         ~currentIndex:num ~env ~full typ
                     in
                     if asSnippet then
                       "${" ^ string_of_int num ^ ":" ^ varName ^ "}"
                     else varName))
          |> String.concat ", "
        in
        "(" ^ if shouldPrintAsUncurried then ". " else "" ^ argsText ^ ")"
    in
    let isAsync =
      match TypeUtils.extractType ~env ~package:full.package returnType with
      | Some (Tpromise _, _) -> true
      | _ -> false
    in
    let asyncPrefix = if isAsync then "async " else "" in
    let functionBody, functionBodyInsertText =
      match args with
      | [(Nolabel, argTyp)] ->
        let varName =
          CompletionExpressions.prettyPrintFnTemplateArgName ~env ~full argTyp
        in
        ( (" => " ^ if varName = "()" then "{}" else varName),
          " => ${0:" ^ varName ^ "}" )
      | _ -> (" => {}", " => {${0:()}}")
    in
    [
      create
        (asyncPrefix ^ mkFnArgs ~asSnippet:false ^ functionBody)
        ~includesSnippets:true
        ~insertText:
          (asyncPrefix ^ mkFnArgs ~asSnippet:true ^ functionBodyInsertText)
        ~sortText:"A" ~kind:(Value typ) ~env;
    ]
  | Tfunction _ ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> Tfunction #other";
    []
  | Texn env ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Texn";
    [
      create
        (["Exn"; "Error(error)"] |> ident)
        ~kind:(Label "Catches errors from JavaScript errors.")
        ~docstring:
          [
            "Matches on a JavaScript error. Read more in the [documentation on \
             catching JS \
             exceptions](https://rescript-lang.org/docs/manual/latest/exception#catching-js-exceptions).";
          ]
        ~env;
    ]
  | Tpromise _ ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tpromise";
    []

module StringSet = Set.Make (String)

let rec processCompletable ~debug ~full ~scope ~env ~pos ~forHover completable =
  if debug then
    Printf.printf "Completable: %s\n" (Completable.toString completable);
  let package = full.package in
  let rawOpens = Scope.getRawOpens scope in
  let opens = getOpens ~debug ~rawOpens ~package ~env in
  let allFiles = allFilesInPackage package in
  let findTypeOfValue path =
    path
    |> getCompletionsForPath ~debug ~completionContext:Value ~exact:true ~opens
         ~full ~pos ~env ~scope
    |> completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens ~pos
  in
  match completable with
  | Cnone -> []
  | Cpath contextPath ->
    contextPath
    |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
         ~exact:forHover ~scope
  | Cjsx ([id], prefix, identsSeen) when String.uncapitalize_ascii id = id -> (
    (* Lowercase JSX tag means builtin *)
    let mkLabel (name, typString) =
      Completion.create name ~kind:(Label typString) ~env
    in
    let keyLabels =
      if Utils.startsWith "key" prefix then [mkLabel ("key", "string")] else []
    in
    let pathToElementProps = TypeUtils.pathToElementProps package in
    if Debug.verbose () then
      Printf.printf
        "[completing-lowercase-jsx] Attempting to complete from type at %s\n"
        (pathToElementProps |> String.concat ".");
    let fromElementProps =
      match
        pathToElementProps
        |> digToRecordFieldsForCompletion ~debug ~package ~opens ~full ~pos ~env
             ~scope
      with
      | None -> None
      | Some fields ->
        Some
          (fields
          |> List.filter_map (fun (f : field) ->
                 if
                   Utils.startsWith f.fname.txt prefix
                   && (forHover || not (List.mem f.fname.txt identsSeen))
                 then
                   Some
                     ( f.fname.txt,
                       Shared.typeToString (Utils.unwrapIfOption f.typ) )
                 else None)
          |> List.map mkLabel)
    in
    match fromElementProps with
    | Some elementProps -> elementProps
    | None ->
      if debug then
        Printf.printf
          "[completing-lowercase-jsx] could not find element props to complete \
           from.\n";
      keyLabels)
  | Cjsx (componentPath, prefix, identsSeen) ->
    let labels =
      CompletionJsx.getJsxLabels ~componentPath ~findTypeOfValue ~package
    in
    let mkLabel_ name typString =
      Completion.create name ~kind:(Label typString) ~env
    in
    let mkLabel (name, typ, _env) =
      mkLabel_ name (typ |> Shared.typeToString)
    in
    let keyLabels =
      if Utils.startsWith "key" prefix then [mkLabel_ "key" "string"] else []
    in
    if labels = [] then []
    else
      (labels
      |> List.filter (fun (name, _t, _env) ->
             Utils.startsWith name prefix
             && name <> "key"
             && (forHover || not (List.mem name identsSeen)))
      |> List.map mkLabel)
      @ keyLabels
  | CdecoratorPayload (JsxConfig {prefix; nested}) -> (
    let mkField ~name ~primitive =
      {
        stamp = -1;
        fname = {loc = Location.none; txt = name};
        optional = true;
        typ = Ctype.newconstr primitive [];
        docstring = [];
        deprecated = None;
      }
    in
    let typ : completionType =
      Trecord
        {
          env;
          definition = `NameOnly "jsxConfig";
          fields =
            [
              mkField ~name:"version" ~primitive:Predef.path_int;
              mkField ~name:"module_" ~primitive:Predef.path_string;
              mkField ~name:"mode" ~primitive:Predef.path_string;
            ];
        }
    in
    match typ |> TypeUtils.resolveNested ~env ~full ~nested with
    | None -> []
    | Some (typ, _env, completionContext, typeArgContext) ->
      typ
      |> completeTypedValue ?typeArgContext ~rawOpens ~mode:Expression ~full
           ~prefix ~completionContext)
  | CdecoratorPayload (ModuleWithImportAttributes {prefix; nested}) -> (
    let mkField ~name ~primitive =
      {
        stamp = -1;
        fname = {loc = Location.none; txt = name};
        optional = true;
        typ = Ctype.newconstr primitive [];
        docstring = [];
        deprecated = None;
      }
    in
    let importAttributesConfig : completionType =
      Trecord
        {
          env;
          definition = `NameOnly "importAttributesConfig";
          fields = [mkField ~name:"type_" ~primitive:Predef.path_string];
        }
    in
    let rootConfig : completionType =
      Trecord
        {
          env;
          definition = `NameOnly "moduleConfig";
          fields =
            [
              mkField ~name:"from" ~primitive:Predef.path_string;
              mkField ~name:"with" ~primitive:Predef.path_string;
            ];
        }
    in
    let nested, typ =
      match nested with
      | NFollowRecordField {fieldName = "with"} :: rest ->
        (rest, importAttributesConfig)
      | _ -> (nested, rootConfig)
    in
    match typ |> TypeUtils.resolveNested ~env ~full ~nested with
    | None -> []
    | Some (typ, _env, completionContext, typeArgContext) ->
      typ
      |> completeTypedValue ?typeArgContext ~rawOpens ~mode:Expression ~full
           ~prefix ~completionContext)
  | CdecoratorPayload (Module prefix) ->
    let packageJsonPath =
      Utils.findPackageJson (full.package.rootPath |> Uri.fromPath)
    in
    let itemsFromPackageJson =
      match packageJsonPath with
      | None ->
        if debug then
          Printf.printf
            "Did not find package.json, started looking (going upwards) from: %s\n"
            full.package.rootPath;
        []
      | Some path -> (
        match Files.readFile path with
        | None ->
          if debug then print_endline "Could not read package.json";
          []
        | Some s -> (
          match Json.parse s with
          | Some (Object items) ->
            items
            |> List.filter_map (fun (key, t) ->
                   match (key, t) with
                   | ("dependencies" | "devDependencies"), Json.Object o ->
                     Some
                       (o
                       |> List.filter_map (fun (pkgName, _) ->
                              match pkgName with
                              | "rescript" -> None
                              | pkgName -> Some pkgName))
                   | _ -> None)
            |> List.flatten
          | _ ->
            if debug then print_endline "Could not parse package.json";
            []))
    in
    (* TODO: Resolve relatives? *)
    let localItems =
      try
        let files =
          Sys.readdir (Filename.dirname (env.file.uri |> Uri.toPath))
          |> Array.to_list
        in
        (* Try to filter out compiled in source files *)
        let resFiles =
          StringSet.of_list
            (files
            |> List.filter_map (fun f ->
                   if Filename.extension f = ".res" then
                     Some (try Filename.chop_extension f with _ -> f)
                   else None))
        in
        files
        |> List.filter_map (fun fileName ->
               let withoutExtension =
                 try Filename.chop_extension fileName with _ -> fileName
               in
               if
                 String.ends_with fileName ~suffix:package.suffix
                 && resFiles |> StringSet.mem withoutExtension
               then None
               else
                 match Filename.extension fileName with
                 | ".res" | ".resi" | "" -> None
                 | _ -> Some ("./" ^ fileName))
        |> List.sort String.compare
      with _ ->
        if debug then print_endline "Could not read relative directory";
        []
    in
    let items = itemsFromPackageJson @ localItems in
    items
    |> List.filter (fun name -> Utils.startsWith name prefix)
    |> List.map (fun name ->
           let isLocal = Utils.startsWith name "./" in
           Completion.create name
             ~kind:(Label (if isLocal then "Local file" else "Package"))
             ~env)
  | Cdecorator prefix ->
    let mkDecorator (name, docstring, maybeInsertText) =
      {
        (Completion.create name ~synthetic:true ~includesSnippets:true
           ~kind:(Label "") ~env ?insertText:maybeInsertText)
        with
        docstring;
      }
    in
    let isTopLevel = String.starts_with ~prefix:"@" prefix in
    let prefix =
      if isTopLevel then String.sub prefix 1 (String.length prefix - 1)
      else prefix
    in
    let decorators =
      if isTopLevel then CompletionDecorators.toplevel
      else CompletionDecorators.local
    in
    decorators
    |> List.filter (fun (decorator, _, _) -> Utils.startsWith decorator prefix)
    |> List.map (fun (decorator, maybeInsertText, doc) ->
           let parts = String.split_on_char '.' prefix in
           let len = String.length prefix in
           let dec2 =
             if List.length parts > 1 then
               String.sub decorator len (String.length decorator - len)
             else decorator
           in
           (dec2, doc, maybeInsertText))
    |> List.map mkDecorator
  | CnamedArg (cp, prefix, identsSeen) ->
    let labels =
      match
        cp
        |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
             ~exact:true ~scope
        |> completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens ~pos
      with
      | Some (typ, _env) ->
        if debug then
          Printf.printf "Found type for function %s\n"
            (typ |> Shared.typeToString);

        typ
        |> TypeUtils.getArgs ~full ~env
        |> List.filter_map (fun arg ->
               match arg with
               | SharedTypes.Completable.Labelled name, a -> Some (name, a)
               | Optional name, a -> Some (name, a)
               | _ -> None)
      | None -> []
    in
    let mkLabel (name, typ) =
      Completion.create name ~kind:(Label (typ |> Shared.typeToString)) ~env
    in
    labels
    |> List.filter (fun (name, _t) ->
           Utils.startsWith name prefix
           && (forHover || not (List.mem name identsSeen)))
    |> List.map mkLabel
  | Cpattern {contextPath; prefix; nested; fallback; patternMode} -> (
    let fallbackOrEmpty ?items () =
      match (fallback, items) with
      | Some fallback, (None | Some []) ->
        fallback |> processCompletable ~debug ~full ~scope ~env ~pos ~forHover
      | _, Some items -> items
      | None, None -> []
    in
    match
      contextPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens ~pos
    with
    | Some (typ, env) -> (
      match
        typ
        |> TypeUtils.extractType ~env ~package:full.package
        |> Utils.Option.flatMap (fun (typ, typeArgContext) ->
               typ |> TypeUtils.resolveNested ?typeArgContext ~env ~full ~nested)
      with
      | None -> fallbackOrEmpty ()
      | Some (typ, _env, completionContext, typeArgContext) ->
        let items =
          typ
          |> completeTypedValue ?typeArgContext ~rawOpens
               ~mode:(Pattern patternMode) ~full ~prefix ~completionContext
        in
        fallbackOrEmpty ~items ())
    | None -> fallbackOrEmpty ())
  | Cexpression {contextPath; prefix; nested} -> (
    let isAmbigiousRecordBodyOrJsxWrap =
      match (contextPath, nested) with
      | CJsxPropValue _, [NRecordBody _] -> true
      | _ -> false
    in
    if Debug.verbose () then
      (* This happens in this scenario: `<SomeComponent someProp={<com>}`
           Here, we don't know whether `{}` is just wraps for the type of
           `someProp`, or if it's a record body where we want to complete
            for the fields in the record. We need to look up what the type is
           first before deciding what completions to show. So we do that here.*)
      if isAmbigiousRecordBodyOrJsxWrap then
        print_endline
          "[process_completable]--> Cexpression special case: JSX prop value \
           that might be record body or JSX wrap"
      else print_endline "[process_completable]--> Cexpression";
    (* Completions for local things like variables in scope, modules in the
       project, etc. We only add completions when there's a prefix of some sort
       we can filter on, since we know we're in some sort of context, and
       therefore don't want to overwhelm the user with completion items. *)
    let regularCompletions =
      if prefix = "" then []
      else
        prefix
        |> getComplementaryCompletionsForTypedValue ~opens ~allFiles ~env ~scope
    in
    match
      contextPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType ~full
    with
    | None ->
      if Debug.verbose () then
        print_endline
          "[process_completable]--> could not get completions for context path";
      regularCompletions
    | Some (typ, env) -> (
      match typ |> TypeUtils.resolveNested ~env ~full ~nested with
      | None ->
        if Debug.verbose () then
          print_endline
            "[process_completable]--> could not resolve nested expression path";
        if isAmbigiousRecordBodyOrJsxWrap then (
          if Debug.verbose () then
            print_endline
              "[process_completable]--> case is ambigious Jsx prop vs record \
               body case, complete also for the JSX prop value directly";
          let itemsForRawJsxPropValue =
            typ
            |> completeTypedValue ~rawOpens ~mode:Expression ~full ~prefix
                 ~completionContext:None
          in
          itemsForRawJsxPropValue @ regularCompletions)
        else regularCompletions
      | Some (typ, _env, completionContext, typeArgContext) -> (
        if Debug.verbose () then
          print_endline
            "[process_completable]--> found type in nested expression \
             completion";
        (* Wrap the insert text in braces when we're completing the root of a
           JSX prop value. *)
        let wrapInsertTextInBraces =
          if List.length nested > 0 then false
          else
            match contextPath with
            | CJsxPropValue _ -> true
            | _ -> false
        in
        let items =
          typ
          |> completeTypedValue ?typeArgContext ~rawOpens ~mode:Expression ~full
               ~prefix ~completionContext
          |> List.map (fun (c : Completion.t) ->
                 if wrapInsertTextInBraces then
                   {
                     c with
                     insertText =
                       (match c.insertText with
                       | None -> None
                       | Some text -> Some ("{" ^ text ^ "}"));
                   }
                 else c)
        in
        match (prefix, completionContext) with
        | "", _ -> items
        | _, None ->
          let items =
            if List.length regularCompletions > 0 then
              (* The client will occasionally sort the list of completions alphabetically, disregarding the order
                 in which we send it. This fixes that by providing a sort text making the typed completions
                 guaranteed to end up on top. *)
              items
              |> List.map (fun (c : Completion.t) ->
                     {c with sortText = Some ("A" ^ " " ^ c.name)})
            else items
          in
          items @ regularCompletions
        | _ -> items)))
  | CexhaustiveSwitch {contextPath; exprLoc} ->
    let range = Utils.rangeOfLoc exprLoc in
    let printFailwithStr num = "${" ^ string_of_int num ^ ":%todo}" in
    let withExhaustiveItem ~cases ?(startIndex = 0) (c : Completion.t) =
      (* We don't need to write out `switch` here since we know that's what the
         user has already written. Just complete for the rest. *)
      let newText =
        c.name ^ " {\n"
        ^ (cases
          |> List.mapi (fun index caseText ->
                 "| " ^ caseText ^ " => "
                 ^ printFailwithStr (startIndex + index + 1))
          |> String.concat "\n")
        ^ "\n}"
        |> Utils.indent range.start.character
      in
      [
        c;
        {
          c with
          name = c.name ^ " (exhaustive switch)";
          filterText = Some c.name;
          insertTextFormat = Some Snippet;
          insertText = Some newText;
          kind = Snippet "insert exhaustive switch for value";
        };
      ]
    in
    let completionsForContextPath =
      contextPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:forHover ~scope
    in
    completionsForContextPath
    |> List.map (fun (c : Completion.t) ->
           match c.kind with
           | Value typExpr -> (
             match typExpr |> TypeUtils.extractType ~env:c.env ~package with
             | Some (Tvariant v, _) ->
               withExhaustiveItem c
                 ~cases:
                   (v.constructors
                   |> List.map (fun (constructor : Constructor.t) ->
                          constructor.cname.txt
                          ^
                          match constructor.args with
                          | Args [] -> ""
                          | _ -> "(_)"))
             | Some (Tpolyvariant v, _) ->
               withExhaustiveItem c
                 ~cases:
                   (v.constructors
                   |> List.map (fun (constructor : polyVariantConstructor) ->
                          "#" ^ constructor.displayName
                          ^
                          match constructor.args with
                          | [] -> ""
                          | _ -> "(_)"))
             | Some (Toption (_env, _typ), _) ->
               withExhaustiveItem c ~cases:["Some($1)"; "None"] ~startIndex:1
             | Some (Tresult _, _) ->
               withExhaustiveItem c ~cases:["Ok($1)"; "Error($1)"] ~startIndex:1
             | Some (Tbool _, _) ->
               withExhaustiveItem c ~cases:["true"; "false"]
             | _ -> [c])
           | _ -> [c])
    |> List.flatten
  | ChtmlElement {prefix} ->
    CompletionJsx.htmlElements
    |> List.filter_map (fun (elementName, description, deprecated) ->
           if Utils.startsWith elementName prefix then
             let name = "<" ^ elementName ^ ">" in
             Some
               (Completion.create name ~synthetic:true ~kind:(Label name)
                  ~detail:description ~env ~docstring:[description]
                  ~insertText:elementName
                  ?deprecated:
                    (match deprecated with
                    | true -> Some "true"
                    | false -> None))
           else None)
  | CextensionNode prefix ->
    if Utils.startsWith "todo" prefix then
      let detail =
        "`%todo` is used to tell the compiler that some code still needs to be \
         implemented."
      in
      [
        Completion.create "todo" ~synthetic:true ~kind:(Label "todo") ~detail
          ~env ~insertText:"todo";
        Completion.create "todo (with payload)" ~synthetic:true
          ~includesSnippets:true ~kind:(Label "todo")
          ~detail:(detail ^ " With a payload.")
          ~env ~insertText:"todo(\"${0:TODO}\")";
      ]
    else []
