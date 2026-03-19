(** LLM Index — extract module information for the LLM SQLite index.

    This module implements the `llmIndex` subcommand for the analysis binary.
    It reads a package context + file list from stdin JSON, walks the
    .cmt/.cmti files, and outputs structured JSON to stdout for insertion
    into a SQLite database. *)

(** Count the number of arrow types (=> in ReScript) in a type expression.
    This gives the number of parameters for a function value. *)
let rec countArrows (typ : Types.type_expr) =
  match typ.desc with
  | Tlink t | Tsubst t | Tpoly (t, []) -> countArrows t
  | Tarrow (_, rest, _, _) -> 1 + countArrows rest
  | _ -> 0

(** Extract the final return type of a (possibly curried) function type. *)
let rec extractReturnType (typ : Types.type_expr) =
  match typ.desc with
  | Tlink t | Tsubst t | Tpoly (t, []) -> extractReturnType t
  | Tarrow (_, ret, _, _) -> extractReturnType ret
  | _ -> Shared.typeToString typ

(** Get the source file path (.res/.resi) from a paths entry. *)
let sourcePathFromPaths (paths : SharedTypes.paths) =
  match paths with
  | Impl {res; _} -> res
  | IntfAndImpl {resi; _} -> resi
  | Namespace {cmt} -> cmt

(** Check whether a module name refers to a namespace (mlmap) rather than a
    real source module. Namespace entries in pathsForModule use the [Namespace]
    variant. *)
let isNamespaceModule ~(package : SharedTypes.package) moduleName =
  match Hashtbl.find_opt package.pathsForModule moduleName with
  | Some (SharedTypes.Namespace _) -> true
  | _ -> false

(** Resolve a target module reference to the actual leaf module for the LLM index.

    This is an improved version of the resolution logic used elsewhere in the
    analysis (e.g. ProcessExtra, References). The general analysis stores
    external references keyed by the top-level module (e.g. "Stdlib") with the
    submodule encoded in the path (e.g. ["Array"; "find"]). That format is
    deeply wired into hover, go-to-definition, and find-references, so we
    cannot change it without a larger refactor.

    For the LLM index we want a uniform representation: target_module should
    always point to the leaf module, with target_path containing only the
    value/type name. This function handles two cases:

    1. Namespace packages (e.g. @rescript/webapi):
       "WebAPI" + ["Document"; "querySelector"] → ("Document-WebAPI", ["querySelector"])

    2. Re-exporting top-level modules (e.g. @rescript/runtime):
       "Stdlib" + ["Array"; "find"] → ("Stdlib_Array", ["find"])
       This works because the runtime ships flat files like Stdlib_Array.resi
       that are re-exported by Stdlib.res.

    TODO: Ideally the core analysis (ProcessExtra/ResolvePath) should resolve
    to leaf modules too, which would make this function unnecessary. That
    requires updating all consumers (hover, references, completion) in one go. *)
let resolveTargetForIndex ~(package : SharedTypes.package) targetModule path =
  match path with
  | firstSegment :: restPath ->
    if isNamespaceModule ~package targetModule then
      (* Case 1: namespace package — "WebAPI" + ["Document", ...] → "Document-WebAPI" *)
      let namespacedName = firstSegment ^ "-" ^ targetModule in
      if Hashtbl.mem package.pathsForModule namespacedName then
        (namespacedName, restPath)
      else (targetModule, path)
    else
      (* Case 2: re-exporting module — "Stdlib" + ["Array", ...] → "Stdlib_Array" *)
      let flatName = targetModule ^ "_" ^ firstSegment in
      if Hashtbl.mem package.pathsForModule flatName then (flatName, restPath)
      else (targetModule, path)
  | [] -> (targetModule, path)

(** Extract cross-module usages from a module's external references.
    Resolves namespace references to the actual sub-module.
    Emits all external references — the Rust side filters via FK resolution. *)
let extractUsages ~(package : SharedTypes.package) (extra : SharedTypes.extra) =
  extra.externalReferences |> Hashtbl.to_seq
  |> Seq.flat_map (fun (targetModule, refs) ->
         List.to_seq refs
         |> Seq.map (fun (path, tip, (loc : Location.t)) ->
                let resolvedModule, resolvedPath =
                  resolveTargetForIndex ~package targetModule path
                in
                let pos = loc.loc_start in
                Protocol.stringifyObject
                  [
                    ("targetModule", Some (Protocol.wrapInQuotes resolvedModule));
                    ( "path",
                      Some
                        (Protocol.array
                           (List.map Protocol.wrapInQuotes resolvedPath)) );
                    ( "tip",
                      Some
                        (Protocol.wrapInQuotes (SharedTypes.Tip.toString tip))
                    );
                    ("line", Some (string_of_int (pos.pos_lnum - 1)));
                    ("col", Some (string_of_int (pos.pos_cnum - pos.pos_bol)));
                  ]))
  |> List.of_seq

(** Walk a Module.structure and produce JSON for the LLM index.
    Returns a JSON string representing a single module object.
    [~usages] is an optional pre-rendered JSON array string for top-level modules;
    nested modules don't carry their own usages (they share the parent's extra). *)
let rec extractModuleForIndex ~rootPath ~sourceFilePath ?(modulePath = [])
    ?(usages = None) (structure : SharedTypes.Module.structure) =
  let open SharedTypes in
  let records = ref [] in
  let variants = ref [] in
  let typeAliases = ref [] in
  let values = ref [] in
  let moduleAliases = ref [] in
  let nestedModules = ref [] in

  structure.items
  |> List.iter (fun (item : Module.item) ->
         let item =
           {item with name = Ext_ident.unwrap_uppercase_exotic item.name}
         in
         match item.kind with
         | Value typ ->
           let signature = Shared.typeToString typ in
           let paramCount = countArrows typ in
           let returnType =
             if paramCount > 0 then Some (extractReturnType typ) else None
           in
           values :=
             Protocol.stringifyObject
               [
                 ("name", Some (Protocol.wrapInQuotes item.name));
                 ("signature", Some (Protocol.wrapInQuotes signature));
                 ("paramCount", Some (string_of_int paramCount));
                 ( "returnType",
                   match returnType with
                   | Some rt -> Some (Protocol.wrapInQuotes rt)
                   | None -> Some Protocol.null );
               ]
             :: !values
         | Type (typ, _) -> (
           match typ.kind with
           | Record fields ->
             let fieldJsons =
               fields
               |> List.map (fun (field : field) ->
                      Protocol.stringifyObject
                        [
                          ("name", Some (Protocol.wrapInQuotes field.fname.txt));
                          ( "signature",
                            Some
                              (Protocol.wrapInQuotes
                                 (Shared.typeToString
                                    (if field.optional then
                                       Utils.unwrapIfOption field.typ
                                     else field.typ))) );
                          ( "optional",
                            Some (if field.optional then "true" else "false") );
                        ])
             in
             records :=
               Protocol.stringifyObject
                 [
                   ("name", Some (Protocol.wrapInQuotes item.name));
                   ( "signature",
                     Some
                       (Protocol.wrapInQuotes
                          (Shared.declToString item.name typ.decl)) );
                   ("fields", Some (Protocol.array fieldJsons));
                 ]
               :: !records
           | Variant constructors ->
             let ctorJsons =
               constructors
               |> List.map (fun (ctor : Constructor.t) ->
                      Protocol.stringifyObject
                        [
                          ("name", Some (Protocol.wrapInQuotes ctor.cname.txt));
                          ( "signature",
                            Some
                              (Protocol.wrapInQuotes
                                 (CompletionBackEnd.showConstructor ctor)) );
                        ])
             in
             variants :=
               Protocol.stringifyObject
                 [
                   ("name", Some (Protocol.wrapInQuotes item.name));
                   ( "signature",
                     Some
                       (Protocol.wrapInQuotes
                          (Shared.declToString item.name typ.decl)) );
                   ("constructors", Some (Protocol.array ctorJsons));
                 ]
               :: !variants
           | Abstract _ | Open | Tuple _ ->
             typeAliases :=
               Protocol.stringifyObject
                 [
                   ("name", Some (Protocol.wrapInQuotes item.name));
                   ( "signature",
                     Some
                       (Protocol.wrapInQuotes
                          (Shared.declToString item.name typ.decl)) );
                 ]
               :: !typeAliases)
         | Module {type_ = Ident p; isModuleType = false} ->
           (* module M = OtherModule — a module alias *)
           let target = SharedTypes.pathIdentToString p in
           moduleAliases :=
             Protocol.stringifyObject
               [
                 ("name", Some (Protocol.wrapInQuotes item.name));
                 ("target", Some (Protocol.wrapInQuotes target));
               ]
             :: !moduleAliases
         | Module {type_ = Structure m; _} ->
           let childPath = structure.name :: modulePath in
           let nested =
             extractModuleForIndex ~rootPath ~sourceFilePath
               ~modulePath:childPath m
           in
           nestedModules := nested :: !nestedModules
         | Module {type_ = Constraint (Structure _impl, Structure iface); _} ->
           (* module M : SIG = IMPL — prefer the interface *)
           let childPath = structure.name :: modulePath in
           let nested =
             extractModuleForIndex ~rootPath ~sourceFilePath
               ~modulePath:childPath iface
           in
           nestedModules := nested :: !nestedModules
         | Module _ -> ());

  let qualifiedName =
    match modulePath with
    | [] -> structure.name
    | parts -> List.rev (structure.name :: parts) |> String.concat "."
  in
  let usagesField =
    match usages with
    | Some u -> ("usages", Some (Protocol.array u))
    | None -> ("usages", Some (Protocol.array []))
  in
  Protocol.stringifyObject
    [
      ("moduleName", Some (Protocol.wrapInQuotes structure.name));
      ("qualifiedName", Some (Protocol.wrapInQuotes qualifiedName));
      ("sourceFilePath", Some (Protocol.wrapInQuotes sourceFilePath));
      ("records", Some (Protocol.array (List.rev !records)));
      ("variants", Some (Protocol.array (List.rev !variants)));
      ("typeAliases", Some (Protocol.array (List.rev !typeAliases)));
      ("values", Some (Protocol.array (List.rev !values)));
      ("moduleAliases", Some (Protocol.array (List.rev !moduleAliases)));
      ("nestedModules", Some (Protocol.array (List.rev !nestedModules)));
      usagesField;
    ]

let getString key obj =
  match Json.get key obj with
  | Some (Json.String s) -> s
  | _ -> ""

let parsePathsForModule json =
  let pathsForModule = Hashtbl.create 30 in
  (match json with
  | Json.Object items ->
    List.iter
      (fun (moduleName, value) ->
        let paths =
          match Json.get "impl" value with
          | Some impl ->
            Some
              (SharedTypes.Impl
                 {cmt = getString "cmt" impl; res = getString "res" impl})
          | None -> (
            match Json.get "intfAndImpl" value with
            | Some ii ->
              Some
                (SharedTypes.IntfAndImpl
                   {
                     cmti = getString "cmti" ii;
                     resi = getString "resi" ii;
                     cmt = getString "cmt" ii;
                     res = getString "res" ii;
                   })
            | None -> (
              match Json.get "namespace" value with
              | Some ns ->
                Some (SharedTypes.Namespace {cmt = getString "cmt" ns})
              | None -> None))
        in
        match paths with
        | Some p -> Hashtbl.replace pathsForModule moduleName p
        | None -> ())
      items
  | _ -> ());
  pathsForModule

let parseFileSet json =
  match json with
  | Some (Json.Array items) ->
    items
    |> List.filter_map (fun item ->
           match item with
           | Json.String s -> Some s
           | _ -> None)
    |> SharedTypes.FileSet.of_list
  | _ -> SharedTypes.FileSet.empty

let parseOpens json =
  match json with
  | Some (Json.Array items) ->
    items
    |> List.filter_map (fun item ->
           match item with
           | Json.Array strings ->
             Some
               (List.filter_map
                  (fun s ->
                    match s with
                    | Json.String s -> Some s
                    | _ -> None)
                  strings)
           | _ -> None)
  | _ -> []

(** Process a single package entry from the stdin JSON. *)
let processPackageEntry json =
  let rootPath = getString "rootPath" json in
  let suffix =
    match Json.get "suffix" json with
    | Some (Json.String s) -> s
    | _ -> ".js"
  in
  let rescriptVersion =
    match Json.get "rescriptVersion" json with
    | Some (Json.Array [Json.Number major; Json.Number minor]) ->
      (int_of_float major, int_of_float minor)
    | _ -> (13, 0)
  in
  let genericJsxModule =
    match Json.get "genericJsxModule" json with
    | Some (Json.String s) -> Some s
    | _ -> None
  in
  let opens = parseOpens (Json.get "opens" json) in
  let pathsForModule =
    match Json.get "pathsForModule" json with
    | Some obj -> parsePathsForModule obj
    | None -> Hashtbl.create 0
  in
  let projectFiles = parseFileSet (Json.get "projectFiles" json) in
  let dependenciesFiles = parseFileSet (Json.get "dependenciesFiles" json) in
  let package : SharedTypes.package =
    {
      genericJsxModule;
      suffix;
      rootPath;
      projectFiles;
      dependenciesFiles;
      pathsForModule;
      namespace = None;
      opens;
      rescriptVersion;
      autocomplete = Misc.StringMap.empty;
    }
  in
  let files =
    match Json.get "files" json with
    | Some (Json.Array items) ->
      items
      |> List.filter_map (fun item ->
             let moduleName = getString "moduleName" item in
             let cmt = getString "cmt" item in
             let cmti = getString "cmti" item in
             (* For structure: prefer cmti (public API), fall back to cmt.
                For usages: prefer cmt (implementation references). *)
             let structurePath =
               if cmti <> "" then cmti else if cmt <> "" then cmt else ""
             in
             let usagesPath =
               if cmt <> "" then cmt else if cmti <> "" then cmti else ""
             in
             if moduleName <> "" && structurePath <> "" then
               Some (moduleName, structurePath, usagesPath)
             else None)
    | _ -> []
  in
  files
  |> List.filter_map (fun (moduleName, structurePath, usagesPath) ->
         match Cmt.loadFullCmtWithPackage ~path:structurePath ~package with
         | None ->
           prerr_endline ("llmIndex: failed to load cmt for " ^ structurePath);
           None
         | Some full ->
           let sourceFilePath =
             match Hashtbl.find_opt package.pathsForModule moduleName with
             | Some paths ->
               let srcPath = sourcePathFromPaths paths in
               Files.relpath rootPath srcPath
               |> Files.split Filename.dir_sep
               |> String.concat "/"
             | None -> structurePath
           in
           (* Extract usages from the implementation (.cmt) when available,
              since that's where actual dependency references live.
              The interface (.cmti) only has type-level references. *)
           let usages =
             if usagesPath <> structurePath && usagesPath <> "" then
               match Cmt.loadFullCmtWithPackage ~path:usagesPath ~package with
               | Some implFull -> extractUsages ~package implFull.extra
               | None -> extractUsages ~package full.extra
             else extractUsages ~package full.extra
           in
           let moduleJson =
             extractModuleForIndex ~rootPath ~sourceFilePath
               ~usages:(Some usages) full.file.structure
           in
           Some moduleJson)

let command () =
  let input = In_channel.input_all In_channel.stdin in
  match Json.parse input with
  | None ->
    prerr_endline "llmIndex: failed to parse JSON from stdin";
    print_endline "[]"
  | Some json ->
    let packageEntries =
      match Json.get "packages" json with
      | Some (Json.Array items) -> items
      | _ -> []
    in
    let results = packageEntries |> List.concat_map processPackageEntry in
    print_endline (Protocol.array results)
