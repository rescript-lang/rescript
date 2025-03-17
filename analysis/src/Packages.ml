open SharedTypes

(* Creates the `pathsForModule` hashtbl, which maps a `moduleName` to it's `paths` (the ml/re, mli/rei, cmt, and cmti files) *)
let makePathsForModule ~projectFilesAndPaths ~dependenciesFilesAndPaths =
  let pathsForModule = Hashtbl.create 30 in
  dependenciesFilesAndPaths
  |> List.iter (fun (modName, paths) ->
         Hashtbl.replace pathsForModule modName paths);
  projectFilesAndPaths
  |> List.iter (fun (modName, paths) ->
         Hashtbl.replace pathsForModule modName paths);
  pathsForModule

let overrideRescriptVersion = ref None

let getReScriptVersion () =
  match !overrideRescriptVersion with
  | Some overrideRescriptVersion -> overrideRescriptVersion
  | None -> (
    (* TODO: Include patch stuff when needed *)
    let defaultVersion = (11, 0) in
    try
      let value = Sys.getenv "RESCRIPT_VERSION" in
      let version =
        match value |> String.split_on_char '.' with
        | major :: minor :: _rest -> (
          match (int_of_string_opt major, int_of_string_opt minor) with
          | Some major, Some minor -> (major, minor)
          | _ -> defaultVersion)
        | _ -> defaultVersion
      in
      version
    with Not_found -> defaultVersion)

let newBsPackage ~rootPath =
  let rescriptJson = Filename.concat rootPath "rescript.json" in
  let bsconfigJson = Filename.concat rootPath "bsconfig.json" in

  let parseRaw raw =
    let libBs =
      match !Cfg.isDocGenFromCompiler with
      | true -> BuildSystem.getStdlib rootPath
      | false -> BuildSystem.getLibBs rootPath
    in
    match Json.parse raw with
    | Some config -> (
      let namespace = FindFiles.getNamespace config in
      let rescriptVersion = getReScriptVersion () in
      let suffix =
        match config |> Json.get "suffix" with
        | Some (String suffix) -> suffix
        | _ -> ".js"
      in
      let uncurried =
        let ns = config |> Json.get "uncurried" in
        match (rescriptVersion, ns) with
        | (major, _), None when major >= 11 -> Some true
        | _, ns -> Option.bind ns Json.bool
      in
      let genericJsxModule =
        let jsxConfig = config |> Json.get "jsx" in
        match jsxConfig with
        | Some jsxConfig -> (
          match jsxConfig |> Json.get "module" with
          | Some (String m) when String.lowercase_ascii m <> "react" -> Some m
          | _ -> None)
        | None -> None
      in
      let uncurried = uncurried = Some true in
      match libBs with
      | None -> None
      | Some libBs ->
        let cached = Cache.readCache (Cache.targetFileFromLibBs libBs) in
        let projectFiles, dependenciesFiles, pathsForModule =
          match cached with
          | Some cached ->
            ( cached.projectFiles,
              cached.dependenciesFiles,
              cached.pathsForModule )
          | None ->
            let dependenciesFilesAndPaths =
              match FindFiles.findDependencyFiles rootPath config with
              | None -> []
              | Some (_dependencyDirectories, dependenciesFilesAndPaths) ->
                dependenciesFilesAndPaths
            in
            let sourceDirectories =
              FindFiles.getSourceDirectories ~includeDev:true ~baseDir:rootPath
                config
            in
            let projectFilesAndPaths =
              FindFiles.findProjectFiles
                ~public:(FindFiles.getPublic config)
                ~namespace ~path:rootPath ~sourceDirectories ~libBs
            in
            let pathsForModule =
              makePathsForModule ~projectFilesAndPaths
                ~dependenciesFilesAndPaths
            in
            let projectFiles =
              projectFilesAndPaths |> List.map fst |> FileSet.of_list
            in
            let dependenciesFiles =
              dependenciesFilesAndPaths |> List.map fst |> FileSet.of_list
            in
            (projectFiles, dependenciesFiles, pathsForModule)
        in
        Some
          (let opens_from_namespace =
             match namespace with
             | None -> []
             | Some namespace ->
               let cmt = Filename.concat libBs namespace ^ ".cmt" in
               Hashtbl.replace pathsForModule namespace (Namespace {cmt});
               let path = [FindFiles.nameSpaceToName namespace] in
               [path]
           in
           let bind f x = Option.bind x f in
           let bsc_flags =
             Json.get "bsc-flags" config
             |> bind Json.array |> Option.value ~default:[]
           in
           let no_pervasives =
             bsc_flags
             |> List.exists (fun s -> Json.string s = Some "-nopervasives")
           in
           let opens_from_bsc_flags =
             List.fold_left
               (fun opens item ->
                 match item |> Json.string with
                 | None -> opens
                 | Some s -> (
                   let parts = String.split_on_char ' ' s in
                   match parts with
                   | "-open" :: name :: _ ->
                     let path = name |> String.split_on_char '.' in
                     path :: opens
                   | _ -> opens))
               [] bsc_flags
           in
           let opens_from_pervasives =
             if no_pervasives then []
             else [["Stdlib"]; ["Pervasives"; "JsxModules"]]
           in
           let opens =
             opens_from_pervasives @ opens_from_namespace
             |> List.rev_append opens_from_bsc_flags
             |> List.map (fun path -> path @ ["place holder"])
           in
           {
             genericJsxModule;
             suffix;
             rescriptVersion;
             rootPath;
             projectFiles;
             dependenciesFiles;
             pathsForModule;
             opens;
             namespace;
             uncurried;
           }))
    | None -> None
  in

  match Files.readFile rescriptJson with
  | Some raw -> parseRaw raw
  | None -> (
    Log.log ("Unable to read " ^ rescriptJson);
    match Files.readFile bsconfigJson with
    | Some raw -> parseRaw raw
    | None ->
      Log.log ("Unable to read " ^ bsconfigJson);
      None)

let findRoot ~uri packagesByRoot =
  let path = Uri.toPath uri in
  let rec loop path =
    if path = "/" then None
    else if Hashtbl.mem packagesByRoot path then Some (`Root path)
    else if
      Files.exists (Filename.concat path "rescript.json")
      || Files.exists (Filename.concat path "bsconfig.json")
    then Some (`Bs path)
    else
      let parent = Filename.dirname path in
      if parent = path then (* reached root *) None else loop parent
  in
  loop (if Sys.is_directory path then path else Filename.dirname path)

let getPackage ~uri =
  let open SharedTypes in
  if Hashtbl.mem state.rootForUri uri then
    Some (Hashtbl.find state.packagesByRoot (Hashtbl.find state.rootForUri uri))
  else
    match findRoot ~uri state.packagesByRoot with
    | None ->
      Log.log "No root directory found";
      None
    | Some (`Root rootPath) ->
      Hashtbl.replace state.rootForUri uri rootPath;
      Some
        (Hashtbl.find state.packagesByRoot (Hashtbl.find state.rootForUri uri))
    | Some (`Bs rootPath) -> (
      match newBsPackage ~rootPath with
      | None -> None
      | Some package ->
        Hashtbl.replace state.rootForUri uri package.rootPath;
        Hashtbl.replace state.packagesByRoot package.rootPath package;
        Some package)
