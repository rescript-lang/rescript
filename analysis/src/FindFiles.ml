let ifDebug debug name fn v = if debug then Log.log (name ^ ": " ^ fn v)
let ( /+ ) = Filename.concat
let bind f x = Option.bind x f

(* Returns a list of paths, relative to the provided `base` *)
let getSourceDirectories ~includeDev ~baseDir config =
  let rec handleItem current (item : Yojson.Safe.t) =
    match item with
    | `List contents -> List.map (handleItem current) contents |> List.concat
    | `String text -> [current /+ text]
    | `Assoc _ -> (
      let dir =
        item |> YojsonHelpers.get "dir"
        |> bind Yojson.Safe.Util.to_string_option
        |> Option.value ~default:"Must specify directory"
      in
      let typ =
        if includeDev then "lib"
        else
          item |> YojsonHelpers.get "type"
          |> bind Yojson.Safe.Util.to_string_option
          |> Option.value ~default:"lib"
      in

      if typ = "dev" then []
      else
        match item |> YojsonHelpers.get "subdirs" with
        | None | Some (`Bool false) -> [current /+ dir]
        | Some (`Bool true) ->
          Files.collectDirs (baseDir /+ current /+ dir)
          |> List.filter (fun name -> name <> Filename.current_dir_name)
          |> List.map (Files.relpath baseDir)
        | Some item -> (current /+ dir) :: handleItem (current /+ dir) item)
    | _ -> failwith "Invalid subdirs entry"
  in
  match config |> YojsonHelpers.get "sources" with
  | None -> []
  | Some item -> handleItem "" item

let isCompiledFile name =
  Filename.check_suffix name ".cmt" || Filename.check_suffix name ".cmti"

let isImplementation name =
  Filename.check_suffix name ".re"
  || Filename.check_suffix name ".res"
  || Filename.check_suffix name ".ml"

let isInterface name =
  Filename.check_suffix name ".rei"
  || Filename.check_suffix name ".resi"
  || Filename.check_suffix name ".mli"

let isSourceFile name = isImplementation name || isInterface name

let compiledNameSpace name =
  String.split_on_char '-' name
  |> List.map String.capitalize_ascii
  |> String.concat ""
  (* Remove underscores??? Whyyy bucklescript, whyyyy *)
  |> String.split_on_char '_'
  |> String.concat ""

let compiledBaseName ~namespace name =
  Filename.chop_extension name
  ^
  match namespace with
  | None -> ""
  | Some n -> "-" ^ compiledNameSpace n

let getName x =
  Filename.basename x |> Filename.chop_extension |> String.capitalize_ascii

let filterDuplicates cmts =
  (* Remove .cmt's that have .cmti's *)
  let intfs = Hashtbl.create 100 in
  cmts
  |> List.iter (fun path ->
         if
           Filename.check_suffix path ".rei"
           || Filename.check_suffix path ".mli"
           || Filename.check_suffix path ".cmti"
         then Hashtbl.add intfs (getName path) true);
  cmts
  |> List.filter (fun path ->
         not
           ((Filename.check_suffix path ".re"
            || Filename.check_suffix path ".ml"
            || Filename.check_suffix path ".cmt")
           && Hashtbl.mem intfs (getName path)))

let nameSpaceToName n =
  n
  |> Str.split (Str.regexp "[-/@]")
  |> List.map String.capitalize_ascii
  |> String.concat ""

let getNamespace config =
  let ns = config |> YojsonHelpers.get "namespace" in
  let fromString = ns |> bind Yojson.Safe.Util.to_string_option in
  let isNamespaced =
    ns
    |> bind Yojson.Safe.Util.to_bool_option
    |> Option.value ~default:(fromString |> Option.is_some)
  in
  let either x y = if x = None then y else x in
  if isNamespaced then
    let fromName =
      config |> YojsonHelpers.get "name"
      |> bind Yojson.Safe.Util.to_string_option
    in
    either fromString fromName |> Option.map nameSpaceToName
  else None

module StringSet = Set.Make (String)

let getPublic config =
  let public = config |> YojsonHelpers.get "public" in
  match public with
  | None -> None
  | Some public -> (
    match public |> YojsonHelpers.to_list_opt with
    | None -> None
    | Some public ->
      Some
        (public
        |> List.filter_map Yojson.Safe.Util.to_string_option
        |> StringSet.of_list))

let collectFiles directory =
  let allFiles = Files.readDirectory directory in
  let compileds = allFiles |> List.filter isCompiledFile |> filterDuplicates in
  let sources = allFiles |> List.filter isSourceFile |> filterDuplicates in
  compileds
  |> Utils.filterMap (fun path ->
         let modName = getName path in
         let cmt = directory /+ path in
         let resOpt =
           Utils.find
             (fun name ->
               if getName name = modName then Some (directory /+ name) else None)
             sources
         in
         match resOpt with
         | None -> None
         | Some res -> Some (modName, SharedTypes.Impl {cmt; res}))

(* Dependency resolution uses the package graph recorded by the build system in
   .sourcedirs.json when available. If a package is not listed there, analysis
   falls back to walking up node_modules from the project root. *)
let readSourcedirsPackageRoots base =
  let sourceDirsFile = base /+ "lib" /+ "bs" /+ ".sourcedirs.json" in
  let readPackageEntry = function
    | `List [`String name; `String path] ->
      let path = if Filename.is_relative path then base /+ path else path in
      Some (name, path)
    | _ -> None
  in
  match Files.readFile sourceDirsFile with
  | None -> []
  | Some text -> (
    match YojsonHelpers.from_string_opt text with
    | None -> []
    | Some json -> (
      match
        json |> YojsonHelpers.get "pkgs" |> bind YojsonHelpers.to_list_opt
      with
      | None -> []
      | Some packages -> packages |> List.filter_map readPackageEntry))

let findPackageRoot ~base ~sourcedirsPackageRoots name =
  match List.assoc_opt name sourcedirsPackageRoots with
  | Some path when Files.exists path -> Some path
  | _ -> ModuleResolution.resolveNodeModulePath ~startPath:base name

(* returns a list of (absolute path to cmt(i), relative path from base to source file) *)
let findProjectFiles ~public ~namespace ~path ~sourceDirectories ~libBs =
  let dirs =
    sourceDirectories |> List.map (Filename.concat path) |> StringSet.of_list
  in
  let files =
    (* Use maxDepth to prevent infinite recursion where `rescript` depends on `@rescript/runtime`,
       but `@rescript/runtime` also has `rescript` as a dev dependency *)
    dirs |> StringSet.elements
    |> List.map (fun name -> Files.collect ~maxDepth:2 name isSourceFile)
    |> List.concat |> StringSet.of_list
  in
  dirs
  |> ifDebug true "Source directories" (fun s ->
         s |> StringSet.elements |> List.map Utils.dumpPath |> String.concat " ");
  files
  |> ifDebug true "Source files" (fun s ->
         s |> StringSet.elements |> List.map Utils.dumpPath |> String.concat " ");

  let interfaces = Hashtbl.create 100 in
  files
  |> StringSet.iter (fun path ->
         if isInterface path then Hashtbl.replace interfaces (getName path) path);

  let normals =
    files |> StringSet.elements
    |> Utils.filterMap (fun file ->
           if isImplementation file then (
             let moduleName = getName file in
             let resi = Hashtbl.find_opt interfaces moduleName in
             Hashtbl.remove interfaces moduleName;
             let base = compiledBaseName ~namespace (Files.relpath path file) in
             match resi with
             | Some resi ->
               let cmti = (libBs /+ base) ^ ".cmti" in
               let cmt = (libBs /+ base) ^ ".cmt" in
               if Files.exists cmti then
                 if Files.exists cmt then
                   (* Log.log("Intf and impl " ++ cmti ++ " " ++ cmt) *)
                   Some
                     ( moduleName,
                       SharedTypes.IntfAndImpl {cmti; resi; cmt; res = file} )
                 else None
               else (
                 (* Log.log("Just intf " ++ cmti) *)
                 Log.log ("Bad source file (no cmt/cmti/cmi) " ^ (libBs /+ base));
                 None)
             | None ->
               let cmt = (libBs /+ base) ^ ".cmt" in
               if Files.exists cmt then Some (moduleName, Impl {cmt; res = file})
               else (
                 Log.log ("Bad source file (no cmt/cmi) " ^ (libBs /+ base));
                 None))
           else None)
  in
  let result =
    normals
    |> List.filter_map (fun (name, paths) ->
           let originalName = name in
           let name =
             match namespace with
             | None -> name
             | Some namespace -> name ^ "-" ^ namespace
           in
           match public with
           | Some public ->
             if public |> StringSet.mem originalName then Some (name, paths)
             else None
           | None -> Some (name, paths))
  in
  match namespace with
  | None -> result
  | Some namespace ->
    let moduleName = nameSpaceToName namespace in
    let cmt = (libBs /+ namespace) ^ ".cmt" in
    Log.log ("adding namespace " ^ namespace ^ " : " ^ moduleName ^ " : " ^ cmt);
    (moduleName, Namespace {cmt}) :: result

let findDependencyFiles base config =
  let deps =
    match
      ( config
        |> YojsonHelpers.get "dependencies"
        |> bind YojsonHelpers.to_list_opt,
        config
        |> YojsonHelpers.get "bs-dependencies"
        |> bind YojsonHelpers.to_list_opt )
    with
    | None, None -> []
    | Some deps, None | _, Some deps ->
      deps |> List.filter_map Yojson.Safe.Util.to_string_option
  in
  let devDeps =
    match
      ( config
        |> YojsonHelpers.get "dev-dependencies"
        |> bind YojsonHelpers.to_list_opt,
        config
        |> YojsonHelpers.get "bs-dev-dependencies"
        |> bind YojsonHelpers.to_list_opt )
    with
    | None, None -> []
    | Some devDeps, None | _, Some devDeps ->
      devDeps |> List.filter_map (fun x -> Some (Yojson.Safe.Util.to_string x))
  in
  let deps = deps @ devDeps in
  Log.log ("Dependencies: " ^ String.concat " " deps);
  let sourcedirsPackageRoots = readSourcedirsPackageRoots base in
  let depFiles =
    deps
    |> List.map (fun name ->
           let result =
             bind
               (fun path ->
                 let rescriptJsonPath = path /+ "rescript.json" in

                 let parseText text =
                   match YojsonHelpers.from_string_opt text with
                   | Some inner -> (
                     let namespace = getNamespace inner in
                     let sourceDirectories =
                       getSourceDirectories ~includeDev:false ~baseDir:path
                         inner
                     in
                     match BuildSystem.getLibBs path with
                     | None -> None
                     | Some libBs ->
                       let compiledDirectories =
                         sourceDirectories |> List.map (Filename.concat libBs)
                       in
                       let compiledDirectories =
                         match namespace with
                         | None -> compiledDirectories
                         | Some _ -> libBs :: compiledDirectories
                       in
                       let projectFiles =
                         findProjectFiles ~public:(getPublic inner) ~namespace
                           ~path ~sourceDirectories ~libBs
                       in
                       Some (compiledDirectories, projectFiles))
                   | None -> None
                 in

                 match Files.readFile rescriptJsonPath with
                 | Some text -> parseText text
                 | None -> None)
               (findPackageRoot ~base ~sourcedirsPackageRoots name)
           in

           match result with
           | Some (files, directories) -> (files, directories)
           | None ->
             Log.log ("Skipping nonexistent dependency: " ^ name);
             ([], []))
  in
  match BuildSystem.getStdlib base with
  | None -> None
  | Some stdlibDirectory ->
    let compiledDirectories, projectFiles =
      let files, directories = List.split depFiles in
      (List.concat files, List.concat directories)
    in
    let allFiles = projectFiles @ collectFiles stdlibDirectory in
    Some (compiledDirectories, allFiles)
